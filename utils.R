# --- Shiny utils ---
basemap <- function(
                land_sf,
                road_buffer,
                cpra_projects,
                cup,
                ldh,
                oyster_leases,
                aoc,
                # sediment_rate,
                # shallow_cv,
                # deep_cv,
                rc_fetch,
                rc_wlvl,
                rc_road,
                rc_sediment3,
                rc_sediment10,
                mask_open,
                mask_interm,
                # sal3,
                # sal10,
                # depth1,
                # wlvl_clamp,
                hsi_2017,
                # hsi_sal3,
                # hsi_sal10,
                # hsi_sal3_mask,
                # hsi_sal10_mask,
                fetch_cat7,
                waterways,
                sowb,
                pts = NULL
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
  # sal_cols         <- data.frame(numeric_cols = 1:36)
  # sal_pal          <- colorNumeric('viridis', domain = sal_cols$numeric_cols, na.color = NA, reverse = TRUE)
  #
  # # Water level variability palatte + labels
  # wlvl_pal          <- colorNumeric(turbo(n = 20), domain = values(wlvl_clamp), na.color = NA, reverse = F)
  # # wlvl_pal          <- colorNumeric(turbo(n = 20), domain = wlvl_cols$numeric_cols, na.color = NA, reverse = F)
  # # wlvl_cols         <- data.frame(numeric_cols = c(0.05, 0.10, 0.15, 0.20, 0.25))
  #
  # # Depth factors color palette + labels
  # depth_lvl          <- data.frame(numeric_cols = 1:3)
  # depth_labels       <- c("Too shallow", "Shallow water", "Deep water")
  # depth_fact_pal     <- colorFactor(brewer.pal(9, "Spectral"), domain = depth_labels, reverse = T)
  # depth_fact_pal   <- colorNumeric(mako(n = 3), domain = values(depth1), reverse = T)
  # depth_fact_pal   <- colorFactor(brewer.pal(9, "Spectral"), domain = depth_labels, reverse = T)
  # depth_cols         <- data.frame(numeric_cols = 0:6)
  # depth_pal          <- colorNumeric(viridisLite::turbo(n = 6), domain = depth_cols$numeric_cols, reverse = F)

  # LDH factor color palette
  factpal          <- colorFactor(c("red", "yellow", "green"),   domain = ldh$Status)

  # sowb legend color palette + label
  sowb_label       <- "State owned water bottoms"
  sowb_pal         <- colorFactor(c("#00bfb2"),   domain = sowb_label)

  # Coastal use permits legend color palette + label
  cup_label        <- "Coastal use permits"
  cup_pal          <- colorFactor(c("#EAC435"),   domain = cup_label)

  # CPRA restoration proj legend color palette + label
  cpra_proj_label  <- "CPRA projects"
  cpra_proj_pal    <- colorFactor(c("green", "orange"),   domain = cpra_projects$type)
  # cpra_proj_label  <- "CPRA projects"
  # cpra_proj_pal    <- colorFactor(c("hotpink"),   domain = cpra_proj_label)

  # Waterways legend color palette + label
  waterways_label  <- "USACE navigation channels"
  waterways_pal    <- colorFactor(c("dodgerblue"), domain = waterways_label)

  # Mask open legend color palette + label
  mask_open_label          <- "Full mask (LDH open)"
  mask_open_pal            <- colorFactor(c("grey"),   domain = mask_open_label)

  # Mask open + interm. legend color palette + label
  mask_interm_label        <- "Full mask (LDH open & intermed.)"
  mask_interm_pal          <- colorFactor(c("grey"),   domain = mask_interm_label)

  # AOC active/not active factor color palette
  aoc_pal          <- colorFactor(c("darkorange"),   domain = aoc$label)

  # Oyster leases palette
  leases_pal        <- colorFactor(c("hotpink"),   domain = oyster_leases$label)

  # # Land water legend color palette + label
  # land_label        <- "2023 MP Land (Year 1)"
  # land_pal          <- colorFactor(c("grey"),   domain = land_sf$label)

  # HSI 2017 500m grid color + labels
  vect <- 0:1
  hsi_pal           <- colorNumeric(viridis(n =20), domain = vect, na.color = NA,
                                    reverse = F)
  hsi_pal2          <- colorNumeric(viridis(n =20), domain = values(hsi_2017), na.color = NA,
                                    reverse = F)
  # hsi_cols         <- data.frame(numeric_cols = 0:1)
  # hsi_pal          <- colorNumeric('magma', domain =values(hsi_sal3), na.color = NA, reverse = F)

  # commercial viability palettes
  comm_var_pal  <- colorNumeric(turbo(n =20), domain = vect, na.color = NA,
                                reverse = F)
  #
  # # reclass_pal   <- colorNumeric(viridis(n =20), domain = vect, na.color = NA,
  # #                               reverse = F)
  reclass_pal   <- colorNumeric(turbo(n =20, direction = -1), domain = vect, na.color = NA,
                                reverse = F)
  #
  # sedim_pal     <- colorNumeric(turbo(n =20, direction = 1), domain = values(sediment_rate), na.color = NA,
  #                                reverse = F)

  # rc_fetch_pal <- colorFactor(c("red", "green", "cyan"), values(rc_fetch),   na.color = "transparent")
  # rc_wlvl_pal <- colorFactor(c("red", "green", "cyan"), values(rc_wlvl), na.color = "transparent")
  # rc_road_pal <- colorFactor(c("red", "green", "cyan"), values(rc_road), na.color = "transparent")

  # rc_fetch_pal <- c("red", "green", "cyan")

  # unique(values(rc_wlvl))
  # mapview::mapview(shallow_cv)
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
        fillColor        = ~factpal(Status),
        fillOpacity      = 0.2,
        color            = ~factpal(Status),
        highlightOptions = highlightOptions(opacity = 1, weight = 6, bringToFront = TRUE), weight = 3, opacity = 1,
        label            = ~Status,
        group            = "Oyster harvest areas") %>%
    addLegend(
        pal              = factpal,
        position         = "topleft",
        title            ='Oyster harvest areas status',
        values           = ldh$Status,
        group            = "Oyster harvest areas",
        layerId          = "Oyster harvest areas") %>%
    addPolygons(
      data               = oyster_leases,
      fillColor          = "hotpink",
      fillOpacity        = 0.2,
      color              = "hotpink",
      weight             = 2,
      opacity            = 1,
      label              = ~label,
      group              = "Oyster leases",
      highlightOptions   = highlightOptions(
                              opacity      = 1,
                              weight       = 6,
                              bringToFront = TRUE
                              )
      ) %>%
    addLegend(
      pal                = leases_pal,
      position           = "topleft",
      values             =  oyster_leases$label,
      group              =  "Oyster leases",
      layerId            =  "Oyster leases") %>%
    addPolygons(
        data             = aoc,
        fillColor        = ~aoc_pal(label),
        fillOpacity      = 0.2,
        color            = ~aoc_pal(label),
        highlightOptions = highlightOptions(color = "red", opacity = 1, weight = 7, bringToFront = TRUE), weight = 3, opacity = 1,
        label            = ~label, group = "AOC permitted areas") %>%
    addLegend(
        pal              = aoc_pal,
        position         = "topleft",
        title            = "AOC permitted areas",
        # title = htmltools::tags$div('LDH Status', style = 'font-size: 16px; color: black;'),
        values           = aoc$label,
        group            = "AOC permitted areas",
        layerId          = "AOC permitted areas") %>%
    addPolygons(
        data             = mask_interm,
        fillColor        = "white", fillOpacity = 0.5, color = "white", weight = 1.5, opacity = 1,
        highlightOptions = highlightOptions(color = "white", opacity = 1, weight = 2.5, bringToFront = TRUE),
        label            = ~label, group = "Full mask (LDH open & intermed.)" )  %>%
    addPolygons(
        data             = mask_open,
        fillColor        = "white", fillOpacity = 0.5,
        color            = "white",
        weight           = 1.5,
        opacity          = 1,
        highlightOptions = highlightOptions(color = "white", opacity = 1, weight = 2.5, bringToFront = TRUE),
        label            = ~label,
        group            = "Full mask (LDH open)")  %>%
    addRasterImage(
        sowb,
        colors           = "#00bfb2", opacity   = 0.7,
        group            = "State owned water bottoms") %>% # colors  = if (raster::is.factor(fetch_cat7)) "Set1" else "YlGnBu",
    addLegend(
        pal              = sowb_pal,
        position         = "topleft", values    = sowb_label,
        group            = "State owned water bottoms", layerId = "State owned water bottoms") %>%
    addRasterImage(
      cup,
      colors             = "#EAC435",
      opacity            = 0.7,
      group              = "Coastal Use Permits") %>%   # colors  = if (raster::is.factor(fetch_cat7)) "Set1" else "YlGnBu",
    addLegend(
      pal                = cup_pal,
      position           = "topleft",
      values             = cup_label,
      group              = "Coastal Use Permits",
      layerId            = "Coastal Use Permits") %>%
    addPolygons(
      data               = cpra_projects,
      fillColor          = ~cpra_proj_pal(type),
      color              = ~cpra_proj_pal(type),
      fillOpacity        = 0.2,
      weight             = 3,
      opacity            = 1,
      label              = ~proj_name,
      group              = "CPRA projects",
      popup              = paste(
        "<b>Project ID: </b> ", cpra_projects$proj_id, "<br>",
        "<b>Project name: </b>", cpra_projects$proj_name, "<br>",
        "<b>Structure status: </b>", cpra_projects$struc_clas, "<br>",
        "<b>Structure class: </b>", cpra_projects$struc_stat, "<br>",
        "<b>Construction date: </b>", cpra_projects$const_date, "<br>"),
      highlightOptions   = highlightOptions(
        opacity      = 1,
        weight       = 6,
        bringToFront = TRUE
      )) %>%  # colors  = if (raster::is.factor(fetch_cat7)) "Set1" else "YlGnBu",
    addLegend(
      pal                = cpra_proj_pal,
      position           = "topleft",
      values             = cpra_projects$type,
      group              = "CPRA projects",
      layerId            = "CPRA projects"
    ) %>%
    addPolylines(
      data               = road_buffer[4,],
      fillColor          = 'transparent',  col   = "red", opacity  = 1, weight  = 3,
      label              = ~road_buffer$buffer_dist[1], group = "Road buffer 20km",
      highlightOptions   = highlightOptions(color = "red", opacity = 1, weight = 7, bringToFront = TRUE)) %>%
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
      label               = ~label,  group = "USACE navigation channels") %>%
    addLegend(
      pal       = waterways_pal,
      position  = "topleft", values = waterways_label,
      group     = "USACE navigation channels", layerId  = "USACE navigation channels"
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
    addRasterImage(
        fetch_cat7,
        opacity   = 0.8,
        colors    = turbo_pal,
        group     = "Fetch") %>%
    addLegend(
        pal       = fetch_fact,
        position  = "bottomleft", title  = "Fetch", labFormat = labelFormat(suffix = " km"),
        group     = "Fetch", layerId  = "Fetch", values    = fetch_cols$fetch_cols ) %>%
    addRasterImage(
        hsi_2017,
        colors             = hsi_pal2,
        opacity            = 0.8,
        group              = "2017 MP Oyster HSI (FWOA, S04, Year 3)") %>%
    addLegend(
        pal                = hsi_pal,
        title              = "2017 MP Oyster HSI (FWOA, S04, Year 3)", position  = "bottomleft",
        group              = "2017 MP Oyster HSI (FWOA, S04, Year 3)", layerId   = "2017 MP Oyster HSI (FWOA, S04, Year 3)", values = vect) %>%
    addPolygons(
        data               = land_sf,
        fillColor          = 'white', fillOpacity =  0.4,
        col                = "black", opacity  = 1, weight  = 1.5,
        group              = "2023 MP Land (Year 1)", label = ~label) %>%
    addLegend(
        pal                = land_pal,
        position           = "topleft", values = land_label,
        group              = "2023 MP Land (Year 1)",
        layerId            = "2023 MP Land (Year 1)") %>%
    addRasterImage(
        rc_fetch,
        project   = T,
        # colors    = viridis(n =20),
        colors    = turbo(n =20, direction = -1),  # colors = rc_fetch_pal,
        opacity   = 0.7,
        group     = "Fetch CV") %>%
    addLegend(
        pal       = reclass_pal,
        position  = "bottomleft",
        # colors = rc_fetch_pal, values = unique(values(rc_fetch)),labels = na.omit(unique(values(rc_fetch))),
        values    = vect,
        title     = "Fetch CV",
        group     = "Fetch CV",  layerId   = "Fetch CV") %>%
    addRasterImage(
        rc_wlvl,
        project   = T,
        # colors    = viridis(n =20),
        colors    = turbo(n =20, direction = -1),
        opacity   = 0.7,
        group     = "Water level variability CV") %>%
    addLegend(
        pal       = reclass_pal,
        position  = "bottomleft", values    = vect,
        title     = "Water level variability CV",
        group     = "Water level variability CV",  layerId   = "Water level variability CV") %>%
    addRasterImage(
        rc_road,
        project   = T,
        # colors    = viridis(n =20),
        colors    = turbo(n =20, direction = -1),
        opacity   = 0.7,
        group     = "Distance to Roads CV") %>%
    addLegend(
        pal       = reclass_pal,
        position  = "bottomleft", values    = vect,
        title     = "Distance to Roads CV",
        group     = "Distance to Roads CV",  layerId   ="Distance to Roads CV") %>%
    addLayersControl(
      options = layersControlOptions(collapsed = TRUE),
      baseGroups = c("Imagery", "Topographic", "Nat. Geo. Topographic"), overlayGroups = c(
        "Fetch",
        "Road buffer 2km", "Road buffer 5km","Road buffer 10km", "Road buffer 20km",
        "2023 MP Land (Year 1)",
        "Oyster harvest areas",
        "Oyster leases",
        "Coastal Use Permits",
        "CPRA projects",
        "USACE navigation channels",
        "State owned water bottoms",
        "AOC permitted areas",
        "Fetch CV",
        "Water level variability CV",
        "Distance to Roads CV",
        "2017 MP Oyster HSI (FWOA, S04, Year 3)",
        "Full mask (LDH open)",
        "Full mask (LDH open & intermed.)")) %>%
    hideGroup(
      c(
        "AOC permitted areas",
        "Fetch",
        "2017 MP Oyster HSI (FWOA, S04, Year 3)",
        "Road buffer 2km", "Road buffer 5km", "Road buffer 10km", "Road buffer 20km",
        "USACE navigation channels",
        "CPRA projects",
        "Coastal Use Permits",
        "State owned water bottoms",
        "Oyster leases",
        "Fetch CV",
        "Water level variability CV",
        "Distance to Roads CV",
        "Full mask (LDH open)",
        "Full mask (LDH open & intermed.)",
        "2023 MP Land (Year 1)"
        )
    )
    # addRasterImage(
    #   cpra_projects,
    #   colors    = "hotpink", opacity   = 0.7,
    #   group     = "CPRA projects") %>%   # colors  = if (raster::is.factor(fetch_cat7)) "Set1" else "YlGnBu",
    # addLegend(
    #   pal       = cpra_proj_pal,
    #   position  = "topleft", values    = cpra_proj_label,
    #   group     = "CPRA projects",  layerId   = "CPRA projects") %>%
    # addRasterImage(
    #     fetch_cat7,
    #     opacity   = 0.8,
    #     colors    = turbo_pal,
    #     group     = "Fetch") %>%
    # addLegend(
    #     pal       = fetch_fact,
    #     position  = "bottomleft", title  = "Fetch", labFormat = labelFormat(suffix = " km"),
    #     group     = "Fetch", layerId  = "Fetch", values    = fetch_cols$fetch_cols ) %>%=
    # addRasterImage(
    #     sal3,
    #     opacity   = 0.8,
    #     colors    = sal_pal,
    #     group     = "2023 MP Mean annual salinity (Year 1)") %>%
    # addRasterImage(
    #     sal10,
    #     opacity   = 0.8,
    #     colors    = sal_pal,
    #     group     = "2023 MP Mean annual salinity (Year 8)") %>%
    # addLegend(
    #     pal       = sal_pal,
    #     title     = "2023 MP Mean annual salinity (Year 1)", position  = "bottomleft",
    #     labFormat = labelFormat(suffix = " g/L"),
    #     group     = "2023 MP Mean annual salinity (Year 1)",  layerId = "2023 MP Mean annual salinity (Year 1)", values = sal_cols$numeric_cols) %>%
    # addLegend(
    #     pal       = sal_pal,
    #     title     = "2023 MP Mean annual salinity (Year 8)", position  = "bottomleft",
    #     labFormat = labelFormat(suffix = " g/L"),
    #     group     = "2023 MP Mean annual salinity (Year 8)", layerId = "2023 MP Mean annual salinity (Year 8)", values = sal_cols$numeric_cols) %>%
    # addRasterImage(
    #     depth1,
    #     opacity   = 0.8,
    #     colors    =  brewer.pal(9, "Spectral"),  # colors    =  c(brewer.pal(11, "Spectral")[c(2, 6)],"#5E4FA2"),
    #     group     = "2023 MP Depth (Year 1)") %>%
    # addLegend(
    #     pal       = depth_fact_pal,
    #     title     = "2023 MP Depth (Year 1)",  position  = "bottomleft",
    #     group     = "2023 MP Depth (Year 1)",  layerId = "2023 MP Depth (Year 1)", values = depth_labels) %>%
    # addRasterImage(
    #     wlvl_clamp,
    #     colors    = wlvl_pal,
    #     opacity   = 0.8,
    #     group     = "Water level variability (2021)") %>%
    # addLegend(
    #     pal       = wlvl_pal,
    #     labFormat = labelFormat(suffix = " m"),
    #     title     = "Water level variability (2021)", position  = "bottomleft",
    #     group     = "Water level variability (2021)", layerId   = "Water level variability (2021)", values = values(wlvl_clamp)) %>%
    # addRasterImage(
    #     hsi_sal3,
    #     colors    = viridisLite::viridis(n = 20),
    #     opacity   = 0.7,
    #     group     = "2023 MP Oyster HSI (Year 1)") %>%
    # addLegend(
    #     pal       = hsi_pal,
    #     title     = "2023 MP Oyster HSI (Year 1)", position  = "bottomleft",
    #     group     = "2023 MP Oyster HSI (Year 1)", layerId = "2023 MP Oyster HSI (Year 1)", values = values(hsi_sal3)) %>%
    # addRasterImage(
    #     hsi_sal10,
    #     colors    = viridisLite::viridis(n = 20),
    #     opacity   = 0.7,
    #     group     = "2023 MP Oyster HSI (Year 8)") %>%
    # addLegend(
    #     pal       = hsi_pal,
    #     title     = "2023 MP Oyster HSI (Year 8)", position  = "bottomleft",
    #     group     = "2023 MP Oyster HSI (Year 8)", layerId = "2023 MP Oyster HSI (Year 8)", values = values(hsi_sal10)) %>%
    # addRasterImage(
    #     hsi_sal3_mask,
    #     colors    = viridisLite::viridis(n = 20),
    #     opacity   = 0.7,
    #     group     = "2023 MP Oyster HSI (Year 1) full mask") %>%
    # addLegend(
    #     pal       = hsi_pal,
    #     title     = "2023 MP Oyster HSI (Year 1) full mask", position  = "bottomleft",
    #     group     = "2023 MP Oyster HSI (Year 1) full mask", layerId = "2023 MP Oyster HSI (Year 1) full mask", values = values(hsi_sal3)) %>%
    # addRasterImage(
    #     hsi_sal10_mask,
    #     colors    = viridisLite::viridis(n = 20),
    #     opacity   = 0.7,
    #     group     = "2023 MP Oyster HSI (Year 8) full mask") %>%
    # addLegend(
    #     pal       = hsi_pal,
    #     title     = "2023 MP Oyster HSI (Year 8) full mask", position  = "bottomleft",
    #     group     = "2023 MP Oyster HSI (Year 8) full mask", layerId = "2023 MP Oyster HSI (Year 8) full mask", values = values(hsi_sal10)) %>%
    # addRasterImage(
    #     hsi_2017,
    #     colors             = hsi_pal2,
    #     opacity            = 0.8,
    #     group              = "2017 MP Oyster HSI (FWOA, S04, Year 3)") %>%
    # addLegend(
    #     pal                = hsi_pal,
    #     title              = "2017 MP Oyster HSI (FWOA, S04, Year 3)", position  = "bottomleft",
    #     group              = "2017 MP Oyster HSI (FWOA, S04, Year 3)", layerId   = "2017 MP Oyster HSI (FWOA, S04, Year 3)", values = vect) %>%
    # addRasterImage(
    #     sediment_rate,
    #     project   = T,
    #     colors    = turbo(n =20, direction = 1),
    #     opacity   = 0.7,
    #     group     = "Sedimentation rate") %>%
    # addLegend(
    #     pal       = sedim_pal,
    #     position  = "bottomleft",
    #     values    = values(sediment_rate),
    #     title     = "Sedimentation rate",
    #     group     = "Sedimentation rate",  layerId   = "Sedimentation rate") %>%
    #   addRasterImage(
    #     rc_fetch,
    #     project   = T,
    #     # colors    = viridis(n =20),
    #     colors    = turbo(n =20, direction = -1),  # colors = rc_fetch_pal,
    #     opacity   = 0.7,
    #     group     = "Fetch CV") %>%
    #   addLegend(
    #     pal       = reclass_pal,
    #     position  = "bottomleft",
    #     # colors = rc_fetch_pal, values = unique(values(rc_fetch)),labels = na.omit(unique(values(rc_fetch))),
    #     values    = vect,
    #     title     = "Fetch CV",
    #     group     = "Fetch CV",  layerId   = "Fetch CV") %>%
    #   addRasterImage(
    #     rc_wlvl,
    #     project   = T,
    #     # colors    = viridis(n =20),
    #     colors    = turbo(n =20, direction = -1),
    #     opacity   = 0.7,
    #     group     = "Water level variability CV") %>%
    #   addLegend(
    #     pal       = reclass_pal,
    #     position  = "bottomleft", values    = vect,
    #     title     = "Water level variability CV",
    #     group     = "Water level variability CV",  layerId   = "Water level variability CV") %>%
    #   addRasterImage(
    #     rc_road,
    #     project   = T,
    #     # colors    = viridis(n =20),
    #     colors    = turbo(n =20, direction = -1),
    #     opacity   = 0.7,
    #     group     = "Distance to Roads CV") %>%
    #   addLegend(
    #     pal       = reclass_pal,
    #     position  = "bottomleft", values    = vect,
    #     title     = "Distance to Roads CV",
    #     group     = "Distance to Roads CV",  layerId   ="Distance to Roads CV") %>%
    #   addRasterImage(
    #     rc_sediment,
    #     project   = T,
    #     colors    = turbo(n =20, direction = -1),
    #     opacity   = 0.7,
    #     group     = "Sedimentation rate CV") %>%
    #   addLegend(
    #     pal       = reclass_pal,
    #     position  = "bottomleft", values    = vect,
    #     title     = "Sedimentation rate CV",
    #     group     = "Sedimentation rate CV",  layerId   = "Sedimentation rate CV") %>%
    # addRasterImage(
    #   shallow_cv,
    #   project     = T,
    #   colors      = viridisLite::turbo(n = 20),
    #   opacity     = 0.7,
    #   group       = "Shallow water CV") %>%
    # addLegend(
    #   pal         = comm_var_pal,
    #   title       = "Shallow water CV", position  = "topleft",
    #   group       = "Shallow water CV", layerId = "Shallow water CV", values = vect) %>%
    # addRasterImage(
    #   deep_cv,
    #   project     = T,
    #   colors      = viridisLite::turbo(n = 20),
    #   opacity     = 0.7,
    #   group       = "Deep water CV") %>%
    # addLegend(
    #   pal         = comm_var_pal,
    #   title       = "Deep water CV", position  = "topleft",
    #   group       = "Deep water CV", layerId = "Deep water CV", values = vect) %>%
    # addPolylines(
    #   data              = road_buffer[4,],
    #   fillColor         = 'transparent',  col   = "red", opacity  = 1, weight  = 3,
    #   label             = ~road_buffer$buffer_dist[1], group = "Road buffer 20km",
    #   highlightOptions  = highlightOptions(color = "red", opacity = 1, weight = 7, bringToFront = TRUE)) %>%
    # addPolylines(
    #   data               = road_buffer[3,],
    #   fillColor          = 'transparent', col   = "red", opacity = 1, weight  = 3,
    #   label              = ~road_buffer$buffer_dist[2],  group = "Road buffer 10km",
    #   highlightOptions   = highlightOptions(color = "red", opacity = 1, weight = 7, bringToFront = TRUE)) %>%
    # addPolylines(
    #   data               = road_buffer[2,],
    #   fillColor          = 'transparent',  col   = "red", opacity     = 1,  weight  = 3,
    #   label              = ~road_buffer$buffer_dist[3], group = "Road buffer 5km",
    #   highlightOptions   = highlightOptions(color = "red", opacity = 1, weight = 7, bringToFront = TRUE)) %>%
    # addPolylines(
    #   data               = road_buffer[1,],
    #   fillColor          = 'transparent', col  = "red",opacity  = 1, weight  = 3,
    #   label              = ~road_buffer$buffer_dist[4],  group = "Road buffer 2km",
    #   highlightOptions   = highlightOptions(color = "red", opacity = 1, weight = 7, bringToFront = TRUE)) %>%
    # addPolygons(
    #   data                = waterways,
    #   fillColor           = "dodgerblue", weight = 2, fillOpacity = 0.7, color = "white",
    #   highlightOptions    = highlightOptions(color = "white", opacity = 1, weight = 4, bringToFront = TRUE),
    #   label               = ~label,  group = "USACE navigation channels") %>%
    # addLegend(
    #   pal       = waterways_pal,
    #   position  = "topleft", values = waterways_label,
    #   group     = "USACE navigation channels", layerId  = "USACE navigation channels"
    # ) %>%
    # addLegend(
    #   pal       = mask_interm_pal,
    #   position  = "topleft", values = mask_interm_label,
    #   group     = "Full mask (LDH open & intermed.)",  layerId = "Full mask (LDH open & intermed.)"
    # ) %>%
    # addLegend(
    #   pal       = mask_open_pal,
    #   position  = "topleft", values = mask_open_label,
    #   group     = "Full mask (LDH open)", layerId = "Full mask (LDH open)"
    # ) %>%
    # addPolygons(
    #   data               = land_sf,
    #   fillColor          = 'white', fillOpacity =  0.4,
    #   col                = "black", opacity  = 1, weight  = 1.5,
    #   group              = "2023 MP Land (Year 1)", label = ~label) %>%
    # addLegend(
    #   pal                = land_pal,
    #   position           = "topleft", values = land_label,
    #   group              = "2023 MP Land (Year 1)",
    #   layerId            = "2023 MP Land (Year 1)") %>%
    # addLayersControl(
    #     options = layersControlOptions(collapsed = TRUE),
    #     baseGroups = c("Imagery", "Topographic", "Nat. Geo. Topographic"), overlayGroups = c(
    #       "2023 MP Mean annual salinity (Year 1)",
    #       "2023 MP Mean annual salinity (Year 8)",
    #       "Fetch",
    #       "Water level variability (2021)",
    #       "Road buffer 2km", "Road buffer 5km","Road buffer 10km", "Road buffer 20km",
    #       "2023 MP Land (Year 1)",
    #       "2023 MP Depth (Year 1)",
    #       "Oyster harvest areas",
    #       "Oyster leases",
    #       "Coastal Use Permits",
    #       "CPRA projects",
    #       "USACE navigation channels",
    #       "State owned water bottoms",
    #       "AOC permitted areas",
    #       "Sedimentation rate",
    #       "Shallow water CV",
    #       "Deep water CV",
    #       "Fetch CV",
    #       "Water level variability CV",
    #       "Sedimentation rate CV",
    #       "Distance to Roads CV",
    #       "2017 MP Oyster HSI (FWOA, S04, Year 3)",
    #       "2023 MP Oyster HSI (Year 1)",
    #       "2023 MP Oyster HSI (Year 8)",
    #       "2023 MP Oyster HSI (Year 1) full mask",
    #       "2023 MP Oyster HSI (Year 8) full mask",
    #       "Full mask (LDH open)",
    #       "Full mask (LDH open & intermed.)")) %>%
    # hideGroup(
    #   c(
    #     "AOC permitted areas",
    #     "Sedimentation rate",
    #     "Shallow water CV",
    #     "Deep water CV",
    #     "Fetch CV",
    #     "Water level variability CV",
    #     "Sedimentation rate CV",
    #     "Distance to Roads CV",
    #     "2023 MP Mean annual salinity (Year 1)",
    #     "2023 MP Mean annual salinity (Year 8)",
    #     "2023 MP Depth (Year 1)",
    #     "Water level variability (2021)",
    #     "Fetch",
    #     "2017 MP Oyster HSI (FWOA, S04, Year 3)",
    #     "2023 MP Oyster HSI (Year 1)",
    #     "2023 MP Oyster HSI (Year 8)",
    #     "2023 MP Oyster HSI (Year 1) full mask",
    #     "2023 MP Oyster HSI (Year 8) full mask",
    #     "Road buffer 2km", "Road buffer 5km", "Road buffer 10km", "Road buffer 20km",
    #     "USACE navigation channels",
    #     "CPRA projects",
    #     "Coastal Use Permits",
    #     "State owned water bottoms",
    #     "Oyster leases",
    #     "Full mask (LDH open)",
    #     "Full mask (LDH open & intermed.)",
    #     "2023 MP Land (Year 1)")
    #   )
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

# --- Shiny utils ---
basemap2 <- function(
  sal3,
  fetch_cat7,
  wlvl_clamp,
  sediment_rate3,
  # rc_fetch,
  # rc_fetch_deep,
  # rc_wlvl3,
  # rc_road,  # road_buffer,
  # rc_sediment3,
  # shallow_cv3,
  # shallow_cv3_no_roads,
  # deep_cv3,
  land_sf,
  depth1,
  ldh,
  waterways,
  cup,
  cpra_projects,
  sowb,
  aoc,
  oyster_leases,
  hsi_2017,
  # hsi_sal3,
  pts = NULL
  ) {

    # ---- COLOR PALETTES & LABELS -----

  # Fetch color palette + labels
  fetch_categories <- length(unique(raster::values(fetch_cat7))) -1
  fetch_cols       <- data.frame(fetch_cols = c(1, 2, 3, 4, 5, 10, 20))  # fetch_cols <- data.frame(fetch_cols = 1:fetch_categories)
  turbo_pal        <- viridisLite::turbo(n = fetch_categories, direction = -1)
  fetch_fact_df    <- fetch_cols %>% mutate(fetch_cols = factor(fetch_cols))
  fetch_fact       <- colorFactor(turbo_pal,   domain = fetch_fact_df$fetch_cols)
  fetch_labels     <- c("1km", "2m", "3km", "4km", "5km", "10km", "20km")

  # Salinity color palette + labels
  sal_cols         <- data.frame(numeric_cols = 1:36)
  sal_pal          <- colorNumeric('viridis', domain = sal_cols$numeric_cols, na.color = NA, reverse = TRUE)

  #  Water level variability palatte + labels
  wlvl_pal          <- colorNumeric(turbo(n =20, direction = -1), domain = values(wlvl_clamp), na.color = NA, reverse = F)

  #  Depth factors color palette + labels
  depth_lvl          <- data.frame(numeric_cols = 1:3)
  depth_labels       <- c("Too shallow", "Shallow water", "Deep water")
  depth_fact_pal     <- colorFactor(brewer.pal(9, "Spectral"), domain = depth_labels, reverse = T)
  depth_fact_pal   <- colorNumeric(mako(n = 3), domain = values(depth1), reverse = T)
  depth_fact_pal   <- colorFactor(brewer.pal(9, "Spectral"), domain = depth_labels, reverse = T)
  depth_cols         <- data.frame(numeric_cols = 0:6)
  depth_pal          <- colorNumeric(viridisLite::turbo(n = 6), domain = depth_cols$numeric_cols, reverse = F)

  # LDH factor color palette
  factpal          <- colorFactor(c("red", "yellow", "green"),   domain = ldh$Status)

  # sowb legend color palette + label
  sowb_label       <- "State owned water bottoms"
  sowb_pal         <- colorFactor(c("#00bfb2"),   domain = sowb_label)

  # Coastal use permits legend color palette + label
  cup_label        <- "Coastal use permits"
  cup_pal          <- colorFactor(c("#EAC435"),   domain = cup_label)

  # CPRA restoration proj legend color palette + label
  cpra_proj_label  <- "CPRA projects"
  cpra_proj_pal    <- colorFactor(c("green", "orange"),   domain = cpra_projects$type)

  # Waterways legend color palette + label
  waterways_label  <- "USACE navigation channels"
  waterways_pal    <- colorFactor(c("dodgerblue"), domain = waterways_label)

  # AOC active/not active factor color palette
  aoc_pal          <- colorFactor(c("darkorange"),   domain = aoc$label)

  # Oyster leases palette
  leases_pal        <- colorFactor(c("hotpink"),   domain = oyster_leases$label)

  # Land water legend color palette + label
  land_label        <- "2023 MP Land (Year 1)"
  land_pal          <- colorFactor(c("grey"),   domain = land_sf$label)

  # HSI 2017 500m grid color + labels
  # seq(0.2, 1, by = .05)
  # vect <- 0:1
  vect <- seq(0.2, 1, by = .05)
  hsi_pal           <- colorNumeric(turbo(n =20, direction = -1), domain = vect, na.color = NA, reverse = F)
  hsi_pal2          <- colorNumeric(turbo(n =20, direction = -1), domain = values(hsi_2017), na.color = NA, reverse = F)

  # # commercial viability palettes
  # comm_var_pal  <- colorNumeric(turbo(n =20, direction = -1), domain = vect, na.color = NA,
  #                               reverse = F)
  #
  # # reclassified Commericial Viability layers palette
  # reclass_pal   <- colorNumeric(turbo(n =20, direction = -1), domain = vect, na.color = NA, reverse = F)
  #
  # rc_wlvl_pal      <- colorNumeric(c( "red", "#FDE725FF", "#21908CFF"), # mako(n =20, direction = -1),
  #                                  domain = vect, na.color = NA, reverse = F)
  # rc_wlvl_pal <- colorFactor(c("red", "green", "blue"),# cividis(n =3, direction = 1),  na.color = NA , domain = values(rc_wlvl3))

  # sedimentation rate palette
  sedim_pal     <- colorNumeric(turbo(n =20, direction = -1), domain = values(sediment_rate3), na.color = NA,reverse = F)
    # ----------- LEAFLET MAP ----------

    leaflet() %>%
      addProviderTiles(providers$Esri.OceanBasemap, group = "Topographic") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
      addProviderTiles(providers$Esri.DeLorme, group = "Topographic w/ roads") %>%
      addScaleBar("bottomleft") %>%
      addMeasure(position           = "bottomright",
                 primaryLengthUnit  = "feet",
                 primaryAreaUnit    = "sqmiles",
                 activeColor        = "red",
                 completedColor     = "green") %>%
      leafem::addMouseCoordinates() %>%
      setView(lng = -91.47, lat = 30.295, zoom = 8) %>%
      # # setMaxBounds(lng1 = -95, lat1 = 31, lng2=-87, lat2=28) %>%
      addRasterImage(
        sal3,
        opacity   = 0.8,
        colors    = sal_pal,
        group     = "2023 MP Mean annual salinity (Year 1)") %>%
      addRasterImage(
        fetch_cat7,
        opacity   = 0.8,
        colors    = turbo_pal,
        group     = "Fetch") %>%
      addRasterImage(
        wlvl_clamp,
        colors    = wlvl_pal,
        opacity   = 0.8,
        group     = "Water level variability (2021)") %>%
      addRasterImage(
        sediment_rate3,
        project   = T,
        colors    = turbo(n =20, direction = -1),
        opacity   = 0.7,
        group     = "Sedimentation rate (Year 1)") %>%
      # addRasterImage(
      #   rc_fetch,
      #   project   = T,
      #   colors    = turbo(n =20, direction = -1),  # colors = rc_fetch_pal,
      #   opacity   = 0.7,
      #   group     = "Fetch CV") %>%
      # addRasterImage(
      #   rc_fetch_deep,
      #   project   = T,
      #   colors    = turbo(n =20, direction = -1),  # colors = rc_fetch_pal,
      #   opacity   = 0.7,
      #   group     = "Fetch CV (Deep)") %>%
      # addRasterImage(
      #   rc_wlvl3,
      #   project   = T,
      #   colors    = turbo(n =20, direction = -1),
      #   # c( "red", "#FDE725FF", "#21908CFF"),
      #   # viridis(n =5, direction = -1), colors = colorFactor(c("green", "red", "cyan"), cividis(n =3, direction = 1), na.color = NA , domain = unique(values(rc_wlvl3))),
      #   opacity   = 0.7,
      #   group     = "Water level variability CV (Year 1)") %>%
      # addRasterImage(
      #   rc_road,
      #   project   = T,
      #   colors    = turbo(n =20, direction = -1),
      #   opacity   = 0.7,
      #   group     = "Distance to Roads CV") %>%
      # addRasterImage(
      #   rc_sediment3,
      #   project   = T,
      #   colors    = turbo(n =20, direction = -1),
      #   opacity   = 0.7,
      #   group     = "Sedimentation rate CV (Year 1)") %>%
      # addRasterImage(
      #   shallow_cv3,
      #   project     = T,
      #   colors      = viridisLite::turbo(n = 20, direction = -1),
      #   opacity     = 0.7,
      #   group       = "Shallow water CV (Year 1)") %>%
      # addRasterImage(
      #   shallow_cv3_no_roads,
      #   project     = T,
      #   colors      = viridisLite::turbo(n = 20, direction = -1),
      #   opacity     = 0.7,
      #   group       = "Shallow water CV (Year 1 - No roads buffer)") %>%
      # addRasterImage(
      #   deep_cv3,
      #   project     = T,
      #   colors      = viridisLite::turbo(n = 20, direction = -1),
      #   opacity     = 0.7,
      #   group       = "Deep water CV (Year 1)") %>%
      addPolygons(
        data        = land_sf,
        fillColor   = 'white', fillOpacity =  0.4,
        col         = "black", opacity  = 1, weight  = 1.5,
        group       = "2023 MP Land (Year 1)", label = ~label) %>%
      addRasterImage(
        depth1,
        opacity   = 0.8,
        colors    =  brewer.pal(9, "Spectral"),  # colors    =  c(brewer.pal(11, "Spectral")[c(2, 6)],"#5E4FA2"),
        group     = "2023 MP Depth (Year 1)") %>%
      addPolygons(
        data        = ldh,
        fillColor   = ~factpal(Status),
        fillOpacity = 0.2,
        color       = ~factpal(Status),
        weight      = 3,
        opacity     = 1,
        label       = ~Status,
        group       = "Oyster harvest areas",
        highlightOptions = highlightOptions(
                              opacity = 1,
                              weight = 6,
                              bringToFront = TRUE)) %>%
      addPolygons(
        data                = waterways,
        fillColor           = "dodgerblue", weight = 2, fillOpacity = 0.7, color = "white",
        highlightOptions    = highlightOptions(color = "white", opacity = 1, weight = 4, bringToFront = TRUE),
        label               = ~label,  group = "USACE navigation channels") %>%
      addRasterImage(
        cup,
        colors             = "#EAC435",
        opacity            = 0.7,
        group              = "Coastal Use Permits") %>%
      addPolygons(
        data               = cpra_projects,
        fillColor          = ~cpra_proj_pal(type),
        color              = ~cpra_proj_pal(type),
        fillOpacity        = 0.2,
        weight             = 3,
        opacity            = 1,
        label              = ~proj_name,
        group              = "CPRA projects",
        popup              = paste(
          "<b>Project ID: </b> ", cpra_projects$proj_id, "<br>",
          "<b>Project name: </b>", cpra_projects$proj_name, "<br>",
          "<b>Structure status: </b>", cpra_projects$struc_clas, "<br>",
          "<b>Structure class: </b>", cpra_projects$struc_stat, "<br>",
          "<b>Construction date: </b>", cpra_projects$const_date, "<br>"),
        highlightOptions   = highlightOptions(
          opacity      = 1,
          weight       = 6,
          bringToFront = TRUE
        )) %>%
      addRasterImage(
          sowb,
          colors           = "#00bfb2", opacity   = 0.7,
          group            = "State owned water bottoms") %>% # colors  = if (raster::is.factor(fetch_cat7)) "Set1" else "YlGnBu",
      addPolygons(
          data             = aoc,
          fillColor        = ~aoc_pal(label),
          fillOpacity      = 0.2,
          color            = ~aoc_pal(label),
          highlightOptions = highlightOptions(color = "red", opacity = 1, weight = 7, bringToFront = TRUE), weight = 3, opacity = 1,
          label            = ~label, group = "AOC permitted areas") %>%
      addPolygons(
          data               = oyster_leases,
          fillColor          = "hotpink",
          fillOpacity        = 0.2,
          color              = "hotpink",
          weight             = 2,
          opacity            = 1,
          label              = ~label,
          group              = "Oyster leases",
          highlightOptions   = highlightOptions(
            opacity      = 1,
            weight       = 6,
            bringToFront = TRUE
          )
        ) %>%
      addRasterImage(
          hsi_2017,
          colors             = hsi_pal2,
          opacity            = 0.8,
          group              = "2017 MP Oyster HSI (FWOA, S04, Year 3)") %>%
      addLegend(
        pal       = sal_pal,
        title     = "2023 MP Mean annual salinity (Year 1)", position  = "bottomleft",
        labFormat = labelFormat(suffix = " g/L"),
        group     = "2023 MP Mean annual salinity (Year 1)",  layerId = "2023 MP Mean annual salinity (Year 1)", values = sal_cols$numeric_cols) %>%
      addLegend(
        pal       = fetch_fact,
        position  = "bottomleft", title  = "Fetch", labFormat = labelFormat(suffix = " km"),
        group     = "Fetch", layerId  = "Fetch", values    = fetch_cols$fetch_cols ) %>%
      addLegend(
        pal       = wlvl_pal,
        labFormat = labelFormat(suffix = " m"),
        title     = "WLV (2021)", position  = "bottomleft",
        group     = "Water level variability (2021)", layerId   = "Water level variability (2021)", values = values(wlvl_clamp)) %>%
      addLegend( # Sedimentation Rate legend
        pal       = sedim_pal,
        position  = "bottomleft",
        values    = values(sediment_rate3),
        title     = "Sedimentation rate (Year 1)",
        group     = "Sedimentation rate (Year 1)",  layerId   = "Sedimentation rate (Year 1)") %>%
      # addLegend(
      #   pal       = reclass_pal,
      #   position  = "bottomleft",
      #   values    = vect,
      #   title     = "Fetch CV",
      #   group     = "Fetch CV",  layerId   = "Fetch CV") %>%
      # addLegend(
      #   pal       = reclass_pal,
      #   position  = "bottomleft",
      #   values    = vect,
      #   title     = "Fetch CV (Deep)",
      #   group     = "Fetch CV (Deep)",  layerId   = "Fetch CV (Deep)") %>%
      # addLegend(
      #   pal       = reclass_pal,         # pal       = rc_wlvl_pal,
      #   position  = "bottomleft", values    = vect,
      #   title     = "WLV CV (Year 1)",
      #   group     = "Water level variability CV (Year 1)",  layerId   = "Water level variability CV (Year 1)") %>%
      # addLegend(
      #   pal       = reclass_pal,
      #   position  = "bottomleft", values    = vect,
      #   title     = "Distance to Roads CV",
      #   group     = "Distance to Roads CV",  layerId   ="Distance to Roads CV") %>%
      # addLegend( # Sedimentation Rate CV legend
      #   pal       = reclass_pal,
      #   position  = "bottomleft", values    = vect,
      #   title     = "Sedimentation rate CV (Year 1)",
      #   group     = "Sedimentation rate CV (Year 1)",  layerId   = "Sedimentation rate CV (Year 1)") %>%
      # addLegend( # Shallow Water Comm. Viab legend
      #   pal         = comm_var_pal,
      #   title       = "Shallow water CV (Year 1)", position  = "topleft",
      #   group       = "Shallow water CV (Year 1)", layerId = "Shallow water CV (Year 1)", values = vect) %>%
      # addLegend(
      #   pal         = comm_var_pal,
      #   title       = "Shallow water CV (Year 1 - No roads buffer)", position  = "topleft",
      #   group       = "Shallow water CV (Year 1 - No roads buffer)", layerId = "Shallow water CV (Year 1 - No roads buffer)", values = vect) %>%
      # addLegend(
      #   pal         = comm_var_pal,
      #   title       = "Deep water CV (Year 1)", position  = "topleft",
      #   group       = "Deep water CV (Year 1)", layerId = "Deep water CV (Year 1)", values = vect) %>%
      addLegend(
        pal         = land_pal,
        position    = "topleft", values = land_label,
        group       = "2023 MP Land (Year 1)",
        layerId     = "2023 MP Land (Year 1)") %>%
      addLegend(
        pal         = depth_fact_pal,
        title       = "2023 MP Depth (Year 1)",  position  = "bottomleft",
        group       = "2023 MP Depth (Year 1)",  layerId = "2023 MP Depth (Year 1)", values = depth_labels) %>%
      addLegend(
        pal         = factpal,
        position    = "topleft",
        title       ='Oyster harvest areas status',
        values      = ldh$Status,
        group       = "Oyster harvest areas",
        layerId     = "Oyster harvest areas") %>%
      addLegend(
        pal         = waterways_pal,
        position    = "topleft", values = waterways_label,
        group       = "USACE navigation channels", layerId  = "USACE navigation channels") %>%
      addLegend(
        pal         = cup_pal,
        position    = "topleft",
        values      = cup_label,
        group       = "Coastal Use Permits",
        layerId     = "Coastal Use Permits") %>%
      addLegend(
        pal         = cpra_proj_pal,
        position    = "topleft",
        values      = cpra_projects$type,
        group       = "CPRA projects",
        layerId     = "CPRA projects"
      ) %>%
      addLegend(
        pal              = sowb_pal,
        position         = "topleft", values    = sowb_label,
        group            = "State owned water bottoms", layerId = "State owned water bottoms") %>%
      addLegend(
        pal              = aoc_pal,
        position         = "topleft",
        title            = "AOC permitted areas",
        values           = aoc$label,
        group            = "AOC permitted areas",
        layerId          = "AOC permitted areas") %>%
      addLegend(
        pal                = leases_pal,
        position           = "topleft",
        values             =  oyster_leases$label,
        group              =  "Oyster leases",
        layerId            =  "Oyster leases") %>%
      addLegend(
        pal                = hsi_pal,
        title              = "2017 MP Oyster HSI (FWOA, S04, Year 3)", position  = "bottomleft",
        group              = "2017 MP Oyster HSI (FWOA, S04, Year 3)", layerId   = "2017 MP Oyster HSI (FWOA, S04, Year 3)", values = vect) %>%
      addLayersControl(
          options = layersControlOptions(collapsed = TRUE),
          baseGroups = c("Topographic", "Imagery", "Topographic w/ roads"),
          overlayGroups = c(
            "2023 MP Mean annual salinity (Year 1)",
            "Fetch",
            "Water level variability (2021)",
            "Sedimentation rate (Year 1)",
            # "Fetch CV",
            # "Fetch CV (Deep)",
            # "Water level variability CV (Year 1)",
            # "Distance to Roads CV",
            # "Sedimentation rate CV (Year 1)",
            # "Shallow water CV (Year 1)",
            # "Shallow water CV (Year 1 - No roads buffer)",
            # "Deep water CV (Year 1)",
            "2023 MP Land (Year 1)",
            "2023 MP Depth (Year 1)",
            "Oyster harvest areas",
            "USACE navigation channels",
            "Coastal Use Permits",
            "CPRA projects",
            "State owned water bottoms",
            "AOC permitted areas",
            "Oyster leases",
            "2017 MP Oyster HSI (FWOA, S04, Year 3)")
          ) %>%
      hideGroup(
        c(
          "2023 MP Mean annual salinity (Year 1)",
          "Fetch",
          "Water level variability (2021)",
          "Sedimentation rate (Year 1)",
          # "Fetch CV",
          # "Fetch CV (Deep)",
          # "Water level variability CV (Year 1)",
          # "Distance to Roads CV",
          # "Sedimentation rate CV (Year 1)",
          # "Shallow water CV (Year 1)",
          # "Shallow water CV (Year 1 - No roads buffer)",
          # "Deep water CV (Year 1)",
          "2023 MP Land (Year 1)",
          "2023 MP Depth (Year 1)",
          # "Oyster harvest areas",
          "USACE navigation channels",
          "Coastal Use Permits",
          "CPRA projects",
          "State owned water bottoms",
          "AOC permitted areas",
          "Oyster leases",
          "2017 MP Oyster HSI (FWOA, S04, Year 3)")
        )
}

# --- Shiny utils ---
raw_data_basemap <- function(
  sal3,
  sal10,
  fetch_cat7,
  wlvl_clamp,
  sediment_rate3,
  sediment_rate10,
  land_sf,
  depth1,
  ldh,
  waterways,
  cup,
  cpra_projects,
  sowb,
  aoc,
  oyster_leases,
  hsi_2017,
  # hsi_sal3,
  pts = NULL
) {

  # ---- COLOR PALETTES & LABELS -----

  # Fetch color palette + labels
  fetch_categories <- length(unique(raster::values(fetch_cat7))) -1
  fetch_cols       <- data.frame(fetch_cols = c(1, 2, 3, 4, 5, 10, 20))  # fetch_cols <- data.frame(fetch_cols = 1:fetch_categories)
  turbo_pal        <- viridisLite::turbo(n = fetch_categories, direction = -1)
  fetch_fact_df    <- fetch_cols %>% mutate(fetch_cols = factor(fetch_cols))
  fetch_fact       <- colorFactor(turbo_pal,   domain = fetch_fact_df$fetch_cols)
  fetch_labels     <- c("1km", "2m", "3km", "4km", "5km", "10km", "20km")

  # Salinity color palette + labels
  sal_cols         <- data.frame(numeric_cols = 1:36)
  sal_pal          <- colorNumeric('viridis', domain = sal_cols$numeric_cols, na.color = NA, reverse = TRUE)

  #  Water level variability palatte + labels
  wlvl_pal          <- colorNumeric(turbo(n = 20), domain = values(wlvl_clamp), na.color = NA, reverse = F)

  #  Depth factors color palette + labels
  depth_lvl          <- data.frame(numeric_cols = 1:3)
  depth_labels       <- c("Too shallow", "Shallow water", "Deep water")
  depth_fact_pal     <- colorFactor(brewer.pal(9, "Spectral"), domain = depth_labels, reverse = T)
  depth_fact_pal   <- colorNumeric(mako(n = 3), domain = values(depth1), reverse = T)
  depth_fact_pal   <- colorFactor(brewer.pal(9, "Spectral"), domain = depth_labels, reverse = T)
  depth_cols         <- data.frame(numeric_cols = 0:6)
  depth_pal          <- colorNumeric(viridisLite::turbo(n = 6), domain = depth_cols$numeric_cols, reverse = F)

  # LDH factor color palette
  factpal          <- colorFactor(c("red", "yellow", "green"),   domain = ldh$Status)

  # sowb legend color palette + label
  sowb_label       <- "State owned water bottoms"
  sowb_pal         <- colorFactor(c("#00bfb2"),   domain = sowb_label)

  # Coastal use permits legend color palette + label
  cup_label        <- "Coastal use permits"
  cup_pal          <- colorFactor(c("#EAC435"),   domain = cup_label)

  # CPRA restoration proj legend color palette + label
  cpra_proj_label  <- "CPRA projects"
  cpra_proj_pal    <- colorFactor(c("green", "orange"),   domain = cpra_projects$type)

  # Waterways legend color palette + label
  waterways_label  <- "USACE navigation channels"
  waterways_pal    <- colorFactor(c("dodgerblue"), domain = waterways_label)

  # AOC active/not active factor color palette
  aoc_pal          <- colorFactor(c("darkorange"),   domain = aoc$label)

  # Oyster leases palette
  leases_pal        <- colorFactor(c("hotpink"),   domain = oyster_leases$label)

  # Land water legend color palette + label
  land_label        <- "2023 MP Land (Year 1)"
  land_pal          <- colorFactor(c("grey"),   domain = land_sf$label)

  # HSI 2017 500m grid color + labels
  # seq(0.2, 1, by = .05)
  # vect <- 0:1
  vect <- seq(0.2, 1, by = .05)
  hsi_pal           <- colorNumeric(viridis(n =20), domain = vect, na.color = NA, reverse = F)
  hsi_pal2          <- colorNumeric(viridis(n =20), domain = values(hsi_2017), na.color = NA, reverse = F)

  # commercial viability palettes
  comm_var_pal  <- colorNumeric(turbo(n =20), domain = vect, na.color = NA,
                                reverse = F)

  # reclassified Commericial Viability layers palette
  reclass_pal   <- colorNumeric(turbo(n =20, direction = 1), domain = vect, na.color = NA, reverse = F)

  rc_wlvl_pal      <- colorNumeric(c( "red", "#FDE725FF", "#21908CFF"), # mako(n =20, direction = -1),
                                   domain = vect, na.color = NA, reverse = F)
  # rc_wlvl_pal <- colorFactor(c("red", "green", "blue"),# cividis(n =3, direction = 1),  na.color = NA , domain = values(rc_wlvl3))

  # sedimentation rate palette
  sedim_pal     <- colorNumeric(turbo(n =20, direction = 1), domain = values(sediment_rate3), na.color = NA,reverse = F)

  # mapview(msk, col.regions = "blue") + rc_sediment3
  # ----------- LEAFLET MAP ----------

  leaflet() %>%
    addProviderTiles(providers$Esri.OceanBasemap, group = "Topographic") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
    addProviderTiles(providers$Esri.DeLorme, group = "Topographic w/ roads") %>%
    addScaleBar("bottomleft") %>%
    addMeasure(position           = "bottomright",
               primaryLengthUnit  = "feet",
               primaryAreaUnit    = "sqmiles",
               activeColor        = "red",
               completedColor     = "green") %>%
    leafem::addMouseCoordinates() %>%
    setView(lng = -91.47, lat = 30.295, zoom = 8) %>%
    # # setMaxBounds(lng1 = -95, lat1 = 31, lng2=-87, lat2=28) %>%
    addRasterImage(
      sal3,
      opacity   = 0.8,
      colors    = sal_pal,
      group     = "2023 MP Mean annual salinity (Year 1)") %>%
    addRasterImage(
      sal10,
      opacity   = 0.8,
      colors    = sal_pal,
      group     = "2023 MP Mean annual salinity (Year 8)") %>%
    addRasterImage(
      fetch_cat7,
      opacity   = 0.8,
      colors    = turbo_pal,
      group     = "Fetch") %>%
    addRasterImage(
      wlvl_clamp,
      colors    = wlvl_pal,
      opacity   = 0.8,
      group     = "Water level variability (2021)") %>%
    addRasterImage(
      wlvl_clamp2,
      colors    = wlvl_pal,
      opacity   = 0.8,
      group     = "Water level variability (2028)") %>%
    addRasterImage(
      sediment_rate3,
      project   = T,
      colors    = turbo(n =20, direction = 1),
      opacity   = 0.7,
      group     = "Sedimentation rate (Year 1)") %>%
    addRasterImage(
      sediment_rate10,
      project   = T,
      colors    = turbo(n =20, direction = 1),
      opacity   = 0.7,
      group     = "Sedimentation rate (Year 8)") %>%
    addPolygons(
      data        = land_sf,
      fillColor   = 'white', fillOpacity =  0.4,
      col         = "black", opacity  = 1, weight  = 1.5,
      group       = "2023 MP Land (Year 1)", label = ~label) %>%
    addRasterImage(
      depth1,
      opacity   = 0.8,
      colors    =  brewer.pal(9, "Spectral"),  # colors    =  c(brewer.pal(11, "Spectral")[c(2, 6)],"#5E4FA2"),
      group     = "2023 MP Depth (Year 1)") %>%
    addPolygons(
      data        = ldh,
      fillColor   = ~factpal(Status),
      fillOpacity = 0.2,
      color       = ~factpal(Status),
      weight      = 3,
      opacity     = 1,
      label       = ~Status,
      group       = "Oyster harvest areas",
      highlightOptions = highlightOptions(
        opacity = 1,
        weight = 6,
        bringToFront = TRUE)) %>%
    addPolygons(
      data                = waterways,
      fillColor           = "dodgerblue", weight = 2, fillOpacity = 0.7, color = "white",
      highlightOptions    = highlightOptions(color = "white", opacity = 1, weight = 4, bringToFront = TRUE),
      label               = ~label,  group = "USACE navigation channels") %>%
    addRasterImage(
      cup,
      colors             = "#EAC435",
      opacity            = 0.7,
      group              = "Coastal Use Permits") %>%
    addPolygons(
      data               = cpra_projects,
      fillColor          = ~cpra_proj_pal(type),
      color              = ~cpra_proj_pal(type),
      fillOpacity        = 0.2,
      weight             = 3,
      opacity            = 1,
      label              = ~proj_name,
      group              = "CPRA projects",
      popup              = paste(
        "<b>Project ID: </b> ", cpra_projects$proj_id, "<br>",
        "<b>Project name: </b>", cpra_projects$proj_name, "<br>",
        "<b>Structure status: </b>", cpra_projects$struc_clas, "<br>",
        "<b>Structure class: </b>", cpra_projects$struc_stat, "<br>",
        "<b>Construction date: </b>", cpra_projects$const_date, "<br>"),
      highlightOptions   = highlightOptions(
        opacity      = 1,
        weight       = 6,
        bringToFront = TRUE
      )) %>%
    addRasterImage(
      sowb,
      colors           = "#00bfb2", opacity   = 0.7,
      group            = "State owned water bottoms") %>% # colors  = if (raster::is.factor(fetch_cat7)) "Set1" else "YlGnBu",
    addPolygons(
      data             = aoc,
      fillColor        = ~aoc_pal(label),
      fillOpacity      = 0.2,
      color            = ~aoc_pal(label),
      highlightOptions = highlightOptions(color = "red", opacity = 1, weight = 7, bringToFront = TRUE), weight = 3, opacity = 1,
      label            = ~label, group = "AOC permitted areas") %>%
    addPolygons(
      data               = oyster_leases,
      fillColor          = "hotpink",
      fillOpacity        = 0.2,
      color              = "hotpink",
      weight             = 2,
      opacity            = 1,
      label              = ~label,
      group              = "Oyster leases",
      highlightOptions   = highlightOptions(
        opacity      = 1,
        weight       = 6,
        bringToFront = TRUE
      )
    ) %>%
    addRasterImage(
      hsi_2017,
      colors             = hsi_pal2,
      opacity            = 0.8,
      group              = "2017 MP Oyster HSI (FWOA, S04, Year 3)") %>%
    addLegend(
      pal       = sal_pal,
      title     = "2023 MP Mean annual salinity (Year 1)", position  = "bottomleft",
      labFormat = labelFormat(suffix = " g/L"),
      group     = "2023 MP Mean annual salinity (Year 1)",  layerId = "2023 MP Mean annual salinity (Year 1)", values = sal_cols$numeric_cols) %>%
    addLegend(
      pal       = sal_pal,
      title     = "2023 MP Mean annual salinity (Year 8)", position  = "bottomleft",
      labFormat = labelFormat(suffix = " g/L"),
      group     = "2023 MP Mean annual salinity (Year 8)",  layerId = "2023 MP Mean annual salinity (Year 8)", values = sal_cols$numeric_cols) %>%
    addLegend(
      pal       = fetch_fact,
      position  = "bottomleft", title  = "Fetch", labFormat = labelFormat(suffix = " km"),
      group     = "Fetch", layerId  = "Fetch", values    = fetch_cols$fetch_cols ) %>%
    addLegend(
      pal       = wlvl_pal,
      labFormat = labelFormat(suffix = " m"),
      title     = "WLV (2021)", position  = "bottomleft",
      group     = "Water level variability (2021)", layerId   = "Water level variability (2021)", values = values(wlvl_clamp)) %>%
    addLegend(
      pal       = wlvl_pal,
      labFormat = labelFormat(suffix = " m"),
      title     = "WLV (2028)", position  = "bottomleft",
      group     = "Water level variability (2028)", layerId   = "Water level variability (2028)", values = values(wlvl_clamp2)) %>%
    addLegend( # Sedimentation Rate (Year 1) legend
      pal       = sedim_pal,
      position  = "bottomleft",
      values    = values(sediment_rate3),
      title     = "Sedimentation rate (Year 1)",
      group     = "Sedimentation rate (Year 1)",  layerId   = "Sedimentation rate (Year 1)") %>%
    addLegend( # Sedimentation Rate (Year 8) legend
      pal       = sedim_pal,
      position  = "bottomleft",
      values    = values(sediment_rate10),
      title     = "Sedimentation rate (Year 8)",
      group     = "Sedimentation rate (Year 8)",  layerId   = "Sedimentation rate (Year 8)") %>%

    addLegend(
      pal         = land_pal,
      position    = "topleft", values = land_label,
      group       = "2023 MP Land (Year 1)",
      layerId     = "2023 MP Land (Year 1)") %>%
    addLegend(
      pal         = depth_fact_pal,
      title       = "2023 MP Depth (Year 1)",  position  = "bottomleft",
      group       = "2023 MP Depth (Year 1)",  layerId = "2023 MP Depth (Year 1)", values = depth_labels) %>%
    addLegend(
      pal         = factpal,
      position    = "topleft",
      title       ='Oyster harvest areas status',
      values      = ldh$Status,
      group       = "Oyster harvest areas",
      layerId     = "Oyster harvest areas") %>%
    addLegend(
      pal         = waterways_pal,
      position    = "topleft", values = waterways_label,
      group       = "USACE navigation channels", layerId  = "USACE navigation channels") %>%
    addLegend(
      pal         = cup_pal,
      position    = "topleft",
      values      = cup_label,
      group       = "Coastal Use Permits",
      layerId     = "Coastal Use Permits") %>%
    addLegend(
      pal         = cpra_proj_pal,
      position    = "topleft",
      values      = cpra_projects$type,
      group       = "CPRA projects",
      layerId     = "CPRA projects"
    ) %>%
    addLegend(
      pal              = sowb_pal,
      position         = "topleft", values    = sowb_label,
      group            = "State owned water bottoms", layerId = "State owned water bottoms") %>%
    addLegend(
      pal              = aoc_pal,
      position         = "topleft",
      title            = "AOC permitted areas",
      values           = aoc$label,
      group            = "AOC permitted areas",
      layerId          = "AOC permitted areas") %>%
    addLegend(
      pal                = leases_pal,
      position           = "topleft",
      values             =  oyster_leases$label,
      group              =  "Oyster leases",
      layerId            =  "Oyster leases") %>%
    addLegend(
      pal                = hsi_pal,
      title              = "2017 MP Oyster HSI (FWOA, S04, Year 3)", position  = "bottomleft",
      group              = "2017 MP Oyster HSI (FWOA, S04, Year 3)", layerId   = "2017 MP Oyster HSI (FWOA, S04, Year 3)", values = vect) %>%
    addLayersControl(
      options = layersControlOptions(collapsed = TRUE),
      baseGroups = c("Topographic", "Imagery", "Topographic w/ roads"), overlayGroups = c(
        "2023 MP Mean annual salinity (Year 1)",
        "2023 MP Mean annual salinity (Year 8)",
        "Fetch",
        "Water level variability (2021)",
        "Water level variability (2028)",
        "Sedimentation rate (Year 1)",
        "Sedimentation rate (Year 8)",
        "2023 MP Land (Year 1)",
        "2023 MP Depth (Year 1)",
        "Oyster harvest areas",
        "USACE navigation channels",
        "Coastal Use Permits",
        "CPRA projects",
        "State owned water bottoms",
        "AOC permitted areas",
        "Oyster leases",
        "2017 MP Oyster HSI (FWOA, S04, Year 3)")) %>%
    hideGroup(
      c("2023 MP Mean annual salinity (Year 1)",
        "2023 MP Mean annual salinity (Year 8)",
        "Fetch",
        "Water level variability (2021)",
        "Water level variability (2028)",
        "Sedimentation rate (Year 1)",
        "Sedimentation rate (Year 8)",
        "2023 MP Land (Year 1)",
        "2023 MP Depth (Year 1)",
        "Oyster harvest areas",
        "USACE navigation channels",
        "Coastal Use Permits",
        "CPRA projects",
        "State owned water bottoms",
        "AOC permitted areas",
        "Oyster leases",
        "2017 MP Oyster HSI (FWOA, S04, Year 3)")
      )
}

# --- Shiny utils ---
basemap3 <- function(
  sal10,
  # fetch_cat7,
  wlvl_clamp2,
  sediment_rate10,
  # rc_fetch,
  # rc_fetch_deep,
  # rc_wlvl10,
  # rc_road,  # road_buffer,
  # rc_sediment10,
  # shallow_cv10,
  # shallow_cv10_no_roads,
  # deep_cv10,
  # land_sf,
  # hsi_sal3,
  pts = NULL
) {

  # ---- COLOR PALETTES & LABELS -----

  # Salinity color palette + labels
  sal_cols         <- data.frame(numeric_cols = 1:36)
  sal_pal          <- colorNumeric('viridis', domain = sal_cols$numeric_cols, na.color = NA, reverse = TRUE)

  # Water level variability palatte + labels
  wlvl_pal          <- colorNumeric(turbo(n = 20, direction = -1), domain = values(wlvl_clamp2), na.color = NA, reverse = F)

  # # Land water legend color palette + label
  # land_label        <- "2023 MP Land (Year 1)"
  # land_pal          <- colorFactor(c("grey"),   domain = land_sf$label)

  # HSI 2017 500m grid color + labels
  vect <- seq(0.2, 1, by = .05)
  # commercial viability palettes
  # comm_var_pal  <- colorNumeric(turbo(n =20, direction = -1), domain = vect, na.color = NA,
  #                               reverse = F)
  #
  #
  # # reclassified Commericial Viability layers palette
  # reclass_pal   <- colorNumeric(turbo(n =20, direction = -1), domain = vect, na.color = NA, reverse = F)
  #
  # rc_wlvl_pal   <- colorNumeric(c( "red", "#FDE725FF", "#21908CFF"), # mako(n =20, direction = -1),
  #                                  domain = vect, na.color = NA, reverse = F)
  # rc_wlvl_pal <- colorFactor(c("red", "green", "blue"),# cividis(n =3, direction = 1),  na.color = NA , domain = values(rc_wlvl3))

  # sedimentation rate palette
  sedim_pal     <- colorNumeric(turbo(n =20, direction = -1), domain = values(sediment_rate10), na.color = NA, reverse = F)

  # ----------- LEAFLET MAP ----------

  leaflet() %>%
    addProviderTiles(providers$Esri.OceanBasemap, group = "Topographic") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
    addProviderTiles(providers$Esri.DeLorme, group = "Topographic w/ roads") %>%
    addScaleBar("bottomleft") %>%
    addMeasure(position = "bottomright",
               primaryLengthUnit = "feet",
               primaryAreaUnit = "sqmiles",
               activeColor = "red",
               completedColor = "green") %>%
    leafem::addMouseCoordinates() %>%
    setView(lng = -91.47, lat = 30.295, zoom = 8) %>%
    # setMaxBounds(lng1 = -95, lat1 = 31, lng2=-87, lat2=28) %>%
    addRasterImage(
      sal10,
      opacity   = 0.8,
      colors    = sal_pal,
      group     = "2023 MP Mean annual salinity (Year 8)") %>%
    # addRasterImage(
    #   fetch_cat7,
    #   opacity   = 0.8,
    #   colors    = turbo_pal,
    #   group     = "Fetch") %>%
    addRasterImage(
      wlvl_clamp2,
      colors    = wlvl_pal,
      opacity   = 0.8,
      group     = "Water level variability (2028)") %>%
    addRasterImage(
      sediment_rate10,
      project   = T,
      colors    = turbo(n =20, direction = -1),
      opacity   = 0.7,
      group     = "Sedimentation rate (Year 8)") %>%
    # addRasterImage(
    #   rc_fetch,
    #   project   = T,
    #   colors    = turbo(n =20, direction = -1),  # colors = rc_fetch_pal,
    #   opacity   = 0.7,
    #   group     = "Fetch CV") %>%
    # addRasterImage(
    #   rc_fetch_deep,
    #   project   = T,
    #   colors    = turbo(n =20, direction = -1),  # colors = rc_fetch_pal,
    #   opacity   = 0.7,
    #   group     = "Fetch CV (Deep)") %>%
    # addRasterImage(
    #   rc_wlvl10,
    #   project   = T,
    #   colors    = turbo(n =20, direction = -1),
    #   # c( "red", "#FDE725FF", "#21908CFF"),
    #   opacity   = 0.7,
    #   group     = "Water level variability CV (Year 8)") %>%
    # addRasterImage(
    #   rc_road,
    #   project   = T,
    #   colors    = turbo(n =20, direction = -1),
    #   opacity   = 0.7,
    #   group     = "Distance to Roads CV") %>%
    # addRasterImage(
    #   rc_sediment10,
    #   project   = T,
    #   colors    = turbo(n =20, direction = -1),
    #   opacity   = 0.7,
    #   group     = "Sedimentation rate CV (Year 8)") %>%
    # addRasterImage(
    #   shallow_cv10,
    #   project     = T,
    #   colors      = viridisLite::turbo(n = 20, direction = -1),
    #   opacity     = 0.7,
    #   group       = "Shallow water CV (Year 8)") %>%
    # addRasterImage(
    #   shallow_cv10_no_roads,
    #   project     = T,
    #   colors      = viridisLite::turbo(n = 20, direction = -1),
    #   opacity     = 0.7,
    #   group       = "Shallow water CV (Year 8 - No roads buffer)") %>%
    # addRasterImage(
    #   deep_cv10,
    #   project     = T,
    #   colors      = viridisLite::turbo(n = 20, direction = -1),
    #   opacity     = 0.7,
    #   group       = "Deep water CV (Year 8)") %>%
    # addPolygons(
    #   data        = land_sf,
    #   fillColor   = 'white', fillOpacity =  0.4,
    #   col         = "black", opacity  = 1, weight  = 1.5,
    #   group       = "2023 MP Land (Year 1)", label = ~label) %>%
    addLegend(
      pal       = sal_pal,
      title     = "2023 MP Mean annual salinity (Year 8)", position  = "bottomleft",
      labFormat = labelFormat(suffix = " g/L"),
      group     = "2023 MP Mean annual salinity (Year 8)",  layerId = "2023 MP Mean annual salinity (Year 8)", values = sal_cols$numeric_cols) %>%
    # addLegend(
    #   pal       = fetch_fact,
    #   position  = "bottomleft", title  = "Fetch", labFormat = labelFormat(suffix = " km"),
    #   group     = "Fetch", layerId  = "Fetch", values    = fetch_cols$fetch_cols ) %>%
    addLegend(
      pal       = wlvl_pal,
      labFormat = labelFormat(suffix = " m"),
      title     = "WLV (2028)", position  = "bottomleft",
      group     = "Water level variability (2028)", layerId   = "Water level variability (2028)", values = values(wlvl_clamp2)) %>%
    addLegend( # Sedimentation Rate legend
      pal       = sedim_pal,
      position  = "bottomleft",
      values    = values(sediment_rate10),
      title     = "Sedimentation rate (Year 8)",
      group     = "Sedimentation rate (Year 8)",  layerId   = "Sedimentation rate (Year 8)") %>%
    # addLegend(
    #   pal       = reclass_pal,
    #   position  = "bottomleft",
    #   values    = vect,
    #   title     = "Fetch CV",
    #   group     = "Fetch CV",  layerId   = "Fetch CV") %>%
    # addLegend(
    #   pal       = reclass_pal,
    #   position  = "bottomleft",
    #   values    = vect,
    #   title     = "Fetch CV (Deep)",
    #   group     = "Fetch CV (Deep)",  layerId   = "Fetch CV (Deep)") %>%
    # addLegend(
    #   pal       = reclass_pal,      # pal       = rc_wlvl_pal,
    #   position  = "bottomleft", values    = vect,
    #   title     = "WLV CV (Year 8)",
    #   group     = "Water level variability CV (Year 8)",  layerId   = "Water level variability CV (Year 8)") %>%
    # addLegend(
    #   pal       = reclass_pal,
    #   position  = "bottomleft", values    = vect,
    #   title     = "Distance to Roads CV",
    #   group     = "Distance to Roads CV",  layerId   ="Distance to Roads CV") %>%
    # addLegend( # Sedimentation Rate CV legend
    #   pal       = reclass_pal,
    #   position  = "bottomleft", values    = vect,
    #   title     = "Sedimentation rate CV (Year 8)",
    #   group     = "Sedimentation rate CV (Year 8)",  layerId   = "Sedimentation rate CV (Year 8)") %>%
    # addLegend( # Shallow Water Comm. Viab legend
    #   pal         = comm_var_pal,
    #   title       = "Shallow water CV (Year 8)", position  = "topleft",
    #   group       = "Shallow water CV (Year 8)", layerId = "Shallow water CV (Year 8)", values = vect) %>%
    # addLegend(
    #   pal         = comm_var_pal,
    #   title       = "Shallow water CV (Year 8 - No roads buffer)", position  = "topleft",
    #   group       = "Shallow water CV (Year 8 - No roads buffer)", layerId = "Shallow water CV (Year 8 - No roads buffer)", values = vect) %>%
    # addLegend(
    #   pal         = comm_var_pal,
    #   title       = "Deep water CV (Year 8)", position  = "topleft",
    #   group       = "Deep water CV (Year 8)", layerId = "Deep water CV (Year 8)", values = vect) %>%
    # addLegend(
    #   pal         = land_pal,
    #   position    = "topleft", values = land_label,
    #   group       = "2023 MP Land (Year 1)",
    #   layerId     = "2023 MP Land (Year 1)") %>%
    addLayersControl(
      options = layersControlOptions(collapsed = TRUE),
      baseGroups = c("Topographic", "Imagery", "Topographic w/ roads"),
      overlayGroups = c(
        "2023 MP Mean annual salinity (Year 8)",
        "Water level variability (2028)",
        "Sedimentation rate (Year 8)"
        # "Fetch CV",
        # "Fetch CV (Deep)",
        # "Water level variability CV (Year 8)",
        # "Distance to Roads CV",
        # "Sedimentation rate CV (Year 8)",
        # "Shallow water CV (Year 8)",
        # "Shallow water CV (Year 8 - No roads buffer)",
        # "Deep water CV (Year 8)",
        # "2023 MP Land (Year 1)"
        )
    ) %>%
    hideGroup(
      c(
        "2023 MP Mean annual salinity (Year 8)",
        "Water level variability (2028)",
        "Sedimentation rate (Year 8)"
        # "Fetch CV",
        # "Fetch CV (Deep)",
        # "Water level variability CV (Year 8)",
        # "Distance to Roads CV",
        # "Sedimentation rate CV (Year 8)",
        # "Shallow water CV (Year 8)",
        # "Shallow water CV (Year 8 - No roads buffer)",
        # "Deep water CV (Year 8)",
        # "2023 MP Land (Year 1)"
        )
    )
}

# --- Shiny utils --
cv_basemap <- function(
  rc_fetch,
  rc_fetch_deep,
  rc_wlvl3,
  rc_wlvl10,
  rc_road,  # road_buffer,
  rc_sediment3,
  rc_sediment10,
  shallow_cv3,
  shallow_cv10,
  shallow_cv3_no_roads,
  shallow_cv10_no_roads,
  deep_cv3,
  deep_cv10,
  pts = NULL
) {

  # ---- COLOR PALETTES & LABELS -----


  # 0.2 - 1 values vector
  vect <- seq(0.2, 1, by = .05)

  # commercial viability palettes
  comm_var_pal  <- colorNumeric(turbo(n =20,  direction = -1), domain = vect, na.color = NA, reverse = F)

  # reclassified Commericial Viability layers palette
  reclass_pal   <- colorNumeric(turbo(n =20, direction = -1), domain = vect, na.color = NA, reverse = F)

  # rc_wlvl_pal      <- colorNumeric(c( "red", "#FDE725FF", "#21908CFF"), # mako(n =20, direction = -1),  domain = vect, na.color = NA, reverse = F)
  # rc_wlvl_pal <- colorFactor(c("red", "green", "blue"),# cividis(n =3, direction = -1),  na.color = NA , domain = values(rc_wlvl3))

  # ----------- LEAFLET MAP ----------

  leaflet() %>%
    addProviderTiles(providers$Esri.OceanBasemap, group = "Topographic") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
    addProviderTiles(providers$Esri.DeLorme, group = "Topographic w/ roads") %>%
    addScaleBar("bottomleft") %>%
    addMeasure(position           = "bottomright",
               primaryLengthUnit  = "feet",
               primaryAreaUnit    = "sqmiles",
               activeColor        = "red",
               completedColor     = "green") %>%
    leafem::addMouseCoordinates() %>%
    setView(lng = -91.47, lat = 30.295, zoom = 8) %>%
    # # setMaxBounds(lng1 = -95, lat1 = 31, lng2=-87, lat2=28) %>%
    addRasterImage(
      rc_fetch,
      # fetch_shallow_mask,
      project   = T,
      colors    = turbo(n =20, direction = -1),  # colors = rc_fetch_pal,
      opacity   = 0.7,
      group     = "Fetch CV") %>%
    addRasterImage(
      # fetch_deep_mask,
      rc_fetch_deep,
      project   = T,
      colors    = turbo(n =20, direction = -1),  # colors = rc_fetch_pal,
      opacity   = 0.7,
      group     = "Fetch CV (Deep)") %>%
    addRasterImage(
      rc_wlvl3,
      project   = T,
      colors    = turbo(n =20, direction = -1),
      opacity   = 0.7,
      group     = "Water level variability CV (Year 1)") %>%
    addRasterImage(
      rc_wlvl10,
      project   = T,
      colors    = turbo(n =20, direction = -1),
      opacity   = 0.7,
      group     = "Water level variability CV (Year 8)") %>%
    addRasterImage(
      rc_road,
      project   = T,
      colors    = turbo(n =20, direction = -1),
      opacity   = 0.7,
      group     = "Distance to Roads CV") %>%
    addRasterImage(
      rc_sediment3,
      project   = T,
      colors    = turbo(n =20, direction = -1),
      opacity   = 0.7,
      group     = "Sedimentation rate CV (Year 1)") %>%
    addRasterImage(
      rc_sediment10,
      project   = T,
      colors    = turbo(n =20, direction = -1),
      opacity   = 0.7,
      group     = "Sedimentation rate CV (Year 8)") %>%
    addRasterImage(
      shallow_cv3,
      project     = T,
      colors      = viridisLite::turbo(n = 20, direction = -1),
      opacity     = 0.7,
      group       = "Shallow water CV (Year 1)") %>%
    addRasterImage(
      shallow_cv10,
      project     = T,
      colors      = viridisLite::turbo(n = 20, direction = -1),
      opacity     = 0.7,
      group       = "Shallow water CV (Year 8)") %>%
    addRasterImage(
      shallow_cv3_no_roads,
      project     = T,
      colors      = viridisLite::turbo(n = 20, direction = -1),
      opacity     = 0.7,
      group       = "Shallow water CV (Year 1 - No roads buffer)") %>%
    addRasterImage(
      shallow_cv10_no_roads,
      project     = T,
      colors      = viridisLite::turbo(n = 20, direction = -1),
      opacity     = 0.7,
      group       = "Shallow water CV (Year 8 - No roads buffer)") %>%
    addRasterImage(
      deep_cv3,
      project     = T,
      colors      = viridisLite::turbo(n = 20, direction = -1),
      opacity     = 0.7,
      group       = "Deep water CV (Year 1)") %>%
    addRasterImage(
      deep_cv10,
      project     = T,
      colors      = viridisLite::turbo(n = 20, direction = -1),
      opacity     = 0.7,
      group       = "Deep water CV (Year 8)") %>%
    addLegend(
      pal       = reclass_pal,
      position  = "bottomleft",
      values    = vect,
      title     = "Fetch CV",
      group     = "Fetch CV",  layerId   = "Fetch CV") %>%
    addLegend(
      pal       = reclass_pal,
      position  = "bottomleft",
      values    = vect,
      title     = "Fetch CV (Deep)",
      group     = "Fetch CV (Deep)",  layerId   = "Fetch CV (Deep)") %>%
    addLegend(
      pal       = reclass_pal,         # pal       = rc_wlvl_pal,
      position  = "bottomleft", values    = vect,
      title     = "WLV CV (Year 1)",
      group     = "Water level variability CV (Year 1)",  layerId   = "Water level variability CV (Year 1)") %>%
    addLegend(
      pal       = reclass_pal,         # pal       = rc_wlvl_pal,
      position  = "bottomleft", values    = vect,
      title     = "WLV CV (Year 8)",
      group     = "Water level variability CV (Year 8)",  layerId   = "Water level variability CV (Year 8)") %>%
    addLegend(
      pal       = reclass_pal,
      position  = "bottomleft", values    = vect,
      title     = "Distance to Roads CV",
      group     = "Distance to Roads CV",  layerId   ="Distance to Roads CV") %>%
    addLegend( # Sedimentation Rate CV legend
      pal       = reclass_pal,
      position  = "bottomleft", values    = vect,
      title     = "Sedimentation rate CV (Year 1)",
      group     = "Sedimentation rate CV (Year 1)",  layerId   = "Sedimentation rate CV (Year 1)") %>%
    addLegend( # Sedimentation Rate CV legend
      pal       = reclass_pal,
      position  = "bottomleft", values    = vect,
      title     = "Sedimentation rate CV (Year 8)",
      group     = "Sedimentation rate CV (Year 8)",  layerId   = "Sedimentation rate CV (Year 8)") %>%
    addLegend( # Shallow Water Comm. Viab legend
      pal         = comm_var_pal,
      title       = "Shallow water CV (Year 1)", position  = "topleft",
      group       = "Shallow water CV (Year 1)", layerId = "Shallow water CV (Year 1)", values = vect) %>%
    addLegend( # Shallow Water Comm. Viab legend
      pal         = comm_var_pal,
      title       = "Shallow water CV (Year 8)", position  = "topleft",
      group       = "Shallow water CV (Year 8)", layerId = "Shallow water CV (Year 8)", values = vect) %>%
    addLegend(
      pal         = comm_var_pal,
      title       = "Shallow water CV (Year 1 - No roads buffer)", position  = "topleft",
      group       = "Shallow water CV (Year 1 - No roads buffer)", layerId = "Shallow water CV (Year 1 - No roads buffer)", values = vect) %>%
    addLegend(
      pal         = comm_var_pal,
      title       = "Shallow water CV (Year 8 - No roads buffer)", position  = "topleft",
      group       = "Shallow water CV (Year 8 - No roads buffer)", layerId = "Shallow water CV (Year 8 - No roads buffer)", values = vect) %>%
    addLegend(
      pal         = comm_var_pal,
      title       = "Deep water CV (Year 1)", position  = "topleft",
      group       = "Deep water CV (Year 1)", layerId = "Deep water CV (Year 1)", values = vect) %>%
    addLegend(
      pal         = comm_var_pal,
      title       = "Deep water CV (Year 8)", position  = "topleft",
      group       = "Deep water CV (Year 8)", layerId = "Deep water CV (Year 8)", values = vect) %>%
    addLayersControl(
      options = layersControlOptions(collapsed = TRUE),
      baseGroups = c("Topographic", "Imagery", "Topographic w/ roads"),
      overlayGroups = c(
        "Fetch CV",
        "Fetch CV (Deep)",
        "Water level variability CV (Year 1)",
        "Water level variability CV (Year 8)",
        "Distance to Roads CV",
        "Sedimentation rate CV (Year 1)",
        "Sedimentation rate CV (Year 8)",
        "Shallow water CV (Year 1)",
        "Shallow water CV (Year 8)",
        "Shallow water CV (Year 1 - No roads buffer)",
        "Shallow water CV (Year 8 - No roads buffer)",
        "Deep water CV (Year 1)",
        "Deep water CV (Year 8)")
    ) %>%
    hideGroup(
      c(
        "Fetch CV",
        "Fetch CV (Deep)",
        "Water level variability CV (Year 1)",
        "Water level variability CV (Year 8)",
        "Distance to Roads CV",
        "Sedimentation rate CV (Year 1)",
        "Sedimentation rate CV (Year 8)",
        "Shallow water CV (Year 1)",
        "Shallow water CV (Year 8)",
        "Shallow water CV (Year 1 - No roads buffer)",
        "Shallow water CV (Year 8 - No roads buffer)",
        "Deep water CV (Year 1)",
        "Deep water CV (Year 8)")
    )
}

# --- Shiny utils --
ov_basemap <- function(
  si_cool,
  si_warm,
  si_avg,
  si_ms3,
  si_ms10,
  si_ov3,
  si_ov10,
  pts = NULL
) {

  # ---- COLOR PALETTES & LABELS -----

  # SI value domain
  vect <- seq(0, 1, by = .1)

  # SI palatte
  si_pal          <- colorNumeric(turbo(n =20, direction = -1), domain = vect, na.color = NA, reverse = F)

  # ----------- LEAFLET MAP ----------

  leaflet() %>%
    addProviderTiles(providers$Esri.OceanBasemap, group = "Topographic") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
    addProviderTiles(providers$Esri.DeLorme, group = "Topographic w/ roads") %>%
    addScaleBar("bottomleft") %>%
    addMeasure(position           = "bottomright",
               primaryLengthUnit  = "feet",
               primaryAreaUnit    = "sqmiles",
               activeColor        = "red",
               completedColor     = "green") %>%
    leafem::addMouseCoordinates() %>%
    setView(lng = -91.47, lat = 30.295, zoom = 8) %>%
    # # setMaxBounds(lng1 = -95, lat1 = 31, lng2=-87, lat2=28) %>%
    addRasterImage(
      si_cool$cool_sal_year1,
      project   = T,
      colors    = turbo(n = 20, direction = -1),  # colors = rc_fetch_pal,
      opacity   = 0.7,
      group     = "SI minimum monthly salinity cool months (Year 1)") %>%
    addRasterImage(
      si_cool$cool_sal_year8,
      project   = T,
      colors    = turbo(n = 20, direction = -1),  # colors = rc_fetch_pal,
      opacity   = 0.7,
      group     = "SI minimum monthly salinity cool months (Year 8)") %>%
    addRasterImage(
      si_warm$warm_sal_year1,
      project   = T,
      colors    = turbo(n = 20, direction = -1),  # colors = rc_fetch_pal,
      opacity   = 0.7,
      group     = "SI minimum monthly salinity warm months (Year 1)") %>%
    addRasterImage(
      si_warm$warm_sal_year8,
      project   = T,
      colors    = turbo(n = 20, direction = -1),  # colors = rc_fetch_pal,
      opacity   = 0.7,
      group     = "SI minimum monthly salinity warm months (Year 8)") %>%
    addRasterImage(
      si_avg$avg_sal_year1,
      project   = T,
      colors    = turbo(n = 20, direction = -1),  # colors = rc_fetch_pal,
      opacity   = 0.7,
      group     = "SI salinity average (Year 1)") %>%
    addRasterImage(
      si_avg$avg_sal_year8,
      project   = T,
      colors    = turbo(n = 20, direction = -1),  # colors = rc_fetch_pal,
      opacity   = 0.7,
      group     = "SI salinity average (Year 8)") %>%
    addRasterImage(
      si_ov10,
      project   = T,
      colors    = turbo(n = 20, direction = -1),  # colors = rc_fetch_pal,
      opacity   = 0.7,
      group     = "SI OV (Year 8)") %>%
    addRasterImage(
      si_ov10,
      project   = T,
      colors    = turbo(n = 20, direction = -1),  # colors = rc_fetch_pal,
      opacity   = 0.7,
      group     = "SI OV (Year 8)") %>%
    addRasterImage(
      si_ms3,
      project   = T,
      colors    = turbo(n = 20, direction = -1),  # colors = rc_fetch_pal,
      opacity   = 0.7,
      group     = "SI MS (Year 1)") %>%
    addRasterImage(
      si_ms10,
      project   = T,
      colors    = turbo(n = 20, direction = -1),  # colors = rc_fetch_pal,
      opacity   = 0.7,
      group     = "SI MS (Year 8)") %>%
    addRasterImage(
      si_ov3,
      project   = T,
      colors    = turbo(n = 20, direction = -1),  # colors = rc_fetch_pal,
      opacity   = 0.7,
      group     = "SI OV (Year 1)") %>%
    addRasterImage(
      si_ov10,
      project   = T,
      colors    = turbo(n = 20, direction = -1),  # colors = rc_fetch_pal,
      opacity   = 0.7,
      group     = "SI OV (Year 8)") %>%
    addLegend(
      pal       = si_pal,
      position  = "bottomleft",
      values    = vect,
      title     = "SI min salinity cool months (Year 1)",
      group     = "SI minimum monthly salinity cool months (Year 1)",  layerId   = "SI minimum monthly salinity cool months (Year 1)") %>%
    addLegend(
      pal       = si_pal,
      position  = "bottomleft",
      values    = vect,
      title     = "SI min salinity cool months (Year 8)",
      group     = "SI minimum monthly salinity cool months (Year 8)",  layerId   = "SI minimum monthly salinity cool months (Year 8)") %>%
    addLegend(
      pal       = si_pal,
      position  = "bottomleft",
      values    = vect,
      title     = "SI min salinity warm months (Year 1)",
      group     = "SI minimum monthly salinity warm months (Year 1)",  layerId   = "SI minimum monthly salinity warm months (Year 1)") %>%
    addLegend(
      pal       = si_pal,
      position  = "bottomleft",
      values    = vect,
      title     = "SI min salinity warm months (Year 8)",
      group     = "SI minimum monthly salinity warm months (Year 8)",  layerId   = "SI minimum monthly salinity warm months (Year 8)") %>%
    addLegend(
      pal       = si_pal,
      position  = "bottomleft",
      values    = vect,
      title     = "SI salinity average (Year 1)",
      group     = "SI salinity average (Year 1)",  layerId   = "SI salinity average (Year 1)") %>%
    addLegend(
      pal       = si_pal,
      position  = "bottomleft",
      values    = vect,
      title     = "SI salinity average (Year 8)",
      group     = "SI salinity average (Year 8)",  layerId   = "SI salinity average (Year 8)") %>%
    addLegend(
      pal       = si_pal,
      position  = "topleft",
      values    = vect,
      title     = "SI MS (Year 1)",
      group     = "SI MS (Year 1)",  layerId   = "SI MS (Year 1)") %>%
    addLegend(
      pal       = si_pal,
      position  = "topleft",
      values    = vect,
      title     = "SI MS (Year 8)",
      group     = "SI MS (Year 8)",  layerId   = "SI MS (Year 8)") %>%
    addLegend(
      pal       = si_pal,
      position  = "topleft",
      values    = vect,
      title     = "SI OV (Year 1)",
      group     = "SI OV (Year 1)",  layerId   = "SI OV (Year 1)") %>%
    addLegend(
      pal       = si_pal,
      position  = "topleft",
      values    = vect,
      title     = "SI OV (Year 8)",
      group     = "SI OV (Year 8)",  layerId   = "SI OV (Year 8)") %>%
    addLayersControl(
      options = layersControlOptions(collapsed = TRUE),
      baseGroups = c("Topographic", "Imagery", "Topographic w/ roads"),
      overlayGroups = c(
        "SI minimum monthly salinity cool months (Year 1)",
        "SI minimum monthly salinity cool months (Year 8)",
        "SI minimum monthly salinity warm months (Year 1)",
        "SI minimum monthly salinity warm months (Year 8)",
        "SI salinity average (Year 1)",
        "SI salinity average (Year 8)",
        "SI MS (Year 1)",
        "SI MS (Year 8)",
        "SI OV (Year 1)",
        "SI OV (Year 8)"
        )
    ) %>%
    hideGroup(
      c(
        "SI minimum monthly salinity cool months (Year 1)",
        "SI minimum monthly salinity cool months (Year 8)",
        "SI minimum monthly salinity warm months (Year 1)",
        "SI minimum monthly salinity warm months (Year 8)",
        "SI salinity average (Year 1)",
        "SI salinity average (Year 8)",
        "SI MS (Year 1)",
        "SI MS (Year 8)",
        "SI OV (Year 1)",
        "SI OV (Year 8)"
        )
    )

}

# --- Shiny utils --
aoc_basemap <- function(
  aoc,
  aoc_shallow,
  aoc_no_road ,
  aoc_deep,
  pts = NULL
) {

  # ---- COLOR PALETTES & LABELS -----

  # SI value domain
  vect <- seq(0, 1, by = .1)
  aoc_poly_pal          <- colorFactor(c("darkorange"),   domain = aoc$label)

  # SI palatte
  aoc_pal          <- colorNumeric(turbo(n =20, direction = -1), domain = vect, na.color = NA, reverse = F)

  # ----------- LEAFLET MAP ----------

  leaflet() %>%
    addProviderTiles(providers$Esri.OceanBasemap, group = "Topographic") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
    addProviderTiles(providers$Esri.DeLorme, group = "Topographic w/ roads") %>%
    addScaleBar("bottomleft") %>%
    addMeasure(position           = "bottomright",
               primaryLengthUnit  = "feet",
               primaryAreaUnit    = "sqmiles",
               activeColor        = "red",
               completedColor     = "green") %>%
    leafem::addMouseCoordinates() %>%
    setView(lng = -91.47, lat = 30.295, zoom = 8) %>%
    # # setMaxBounds(lng1 = -95, lat1 = 31, lng2=-87, lat2=28) %>%
    addPolygons(
      data             = aoc,
      fillColor        = ~aoc_poly_pal(label),
      fillOpacity      = 0.2,
      color            = ~aoc_poly_pal(label),
      highlightOptions = highlightOptions(color = "red", opacity = 1, weight = 7, bringToFront = TRUE), weight = 3, opacity = 1,
      label            = ~label, group = "AOC permitted areas") %>%
    addRasterImage(
      aoc_shallow$aoc_shallow_year1,
      project   = T,
      colors    = turbo(n = 20, direction = -1),  # colors = rc_fetch_pal,
      opacity   = 0.7,
      group     = "AOC shallow (Year 1)") %>%
    addRasterImage(
      aoc_shallow$aoc_shallow_year8,
      project   = T,
      colors    = turbo(n = 20, direction = -1),  # colors = rc_fetch_pal,
      opacity   = 0.7,
      group     = "AOC shallow (Year 8)") %>%
    addRasterImage(
      aoc_no_road$aoc_shallow_no_roads_year1,
      project   = T,
      colors    = turbo(n = 20, direction = -1),  # colors = rc_fetch_pal,
      opacity   = 0.7,
      group     = "AOC shallow (no roads - Year 1)") %>%
    addRasterImage(
      aoc_no_road$aoc_shallow_no_roads_year8,
      project   = T,
      colors    = turbo(n = 20, direction = -1),  # colors = rc_fetch_pal,
      opacity   = 0.7,
      group     = "AOC shallow (no roads - Year 8)") %>%
    addRasterImage(
      aoc_deep$aoc_deep_year1,
      project   = T,
      colors    = turbo(n = 20, direction = -1),  # colors = rc_fetch_pal,
      opacity   = 0.7,
      group     = "AOC deep (Year 1)") %>%
    addRasterImage(
      aoc_deep$aoc_deep_year8,
      project   = T,
      colors    = turbo(n = 20, direction = -1),  # colors = rc_fetch_pal,
      opacity   = 0.7,
      group     = "AOC deep (Year 8)") %>%
    addLegend(
      pal              = aoc_poly_pal,
      position         = "topleft",
      title            = "AOC permitted areas",
      values           = aoc$label,
      group            = "AOC permitted areas",
      layerId          = "AOC permitted areas") %>%
    addLegend(
      pal       = aoc_pal,
      position  = "bottomleft",
      values    = vect,
      title     = "AOC shallow (Year 1)",
      group     = "AOC shallow (Year 1)",  layerId   = "AOC shallow (Year 1)") %>%
    addLegend(
      pal       = aoc_pal,
      position  = "bottomleft",
      values    = vect,
      title     = "AOC shallow (Year 8)",
      group     = "AOC shallow (Year 8)",  layerId   = "AOC shallow (Year 8)") %>%
    addLegend(
      pal       = aoc_pal,
      position  = "bottomleft",
      values    = vect,
      title     = "AOC shallow (no roads - Year 1)",
      group     = "AOC shallow (no roads - Year 1)",  layerId   = "AOC shallow (no roads - Year 1)") %>%
    addLegend(
      pal       = aoc_pal,
      position  = "bottomleft",
      values    = vect,
      title     = "AOC shallow (no roads - Year 8)",
      group     = "AOC shallow (no roads - Year 8)",  layerId   = "AOC shallow (no roads - Year 8)") %>%
    addLegend(
      pal       = aoc_pal,
      position  = "bottomleft",
      values    = vect,
      title     = "AOC deep (Year 1)",
      group     = "AOC deep (Year 1)",  layerId   = "AOC deep (Year 1)") %>%
    addLegend(
      pal       = aoc_pal,
      position  = "bottomleft",
      values    = vect,
      title     = "AOC deep (Year 8)",
      group     = "AOC deep (Year 8)",  layerId   = "AOC deep (Year 8)") %>%
    addLayersControl(
      options = layersControlOptions(collapsed = TRUE),
      baseGroups = c("Topographic", "Imagery", "Topographic w/ roads"),
      overlayGroups = c(
        "AOC permitted areas",
        "AOC shallow (Year 1)",
        "AOC shallow (Year 8)",
        "AOC shallow (no roads - Year 1)",
        "AOC shallow (no roads - Year 8)",
        "AOC deep (Year 1)",
        "AOC deep (Year 8)"
      )
    ) %>%
    hideGroup(
      c(
        "AOC permitted areas",
        "AOC shallow (Year 1)",
        "AOC shallow (Year 8)",
        "AOC shallow (no roads - Year 1)",
        "AOC shallow (no roads - Year 8)",
        "AOC deep (Year 1)",
        "AOC deep (Year 8)"
      ))
}
# --- Shiny utils ---
model_map1 <- function(
  land_sf,
  sediment_rate3,
  shallow_cv3,
  shallow_cv3_no_roads,
  deep_cv3,
  rc_fetch,
  rc_wlvl,
  rc_road,
  rc_sediment3,
  sal3,
  depth1,
  wlvl_clamp,
  hsi_sal3,
  hsi_sal3_mask,
  pts = NULL
) {

  # ==================================
  # ---- COLOR PALETTES & LABELS -----
  # ==================================

  # Salinity color palette + labels
  sal_cols         <- data.frame(numeric_cols = 1:36)
  sal_pal          <- colorNumeric('viridis', domain = sal_cols$numeric_cols, na.color = NA, reverse = TRUE)

  # Water level variability palatte + labels
  wlvl_pal          <- colorNumeric(turbo(n = 20), domain = values(wlvl_clamp), na.color = NA, reverse = F)

  # Depth factors color palette + labels
  depth_lvl          <- data.frame(numeric_cols = 1:3)
  depth_labels       <- c("Too shallow", "Shallow water", "Deep water")
  depth_fact_pal     <- colorFactor(brewer.pal(9, "Spectral"), domain = depth_labels, reverse = T)


  # Land water legend color palette + label
  land_label        <- "2023 MP Land (Year 1)"
  land_pal          <- colorFactor(c("grey"),   domain = land_sf$label)

  # HSI 2017 500m grid color + labels
  vect <- 0:1
  hsi_pal           <- colorNumeric(viridis(n =20), domain = vect, na.color = NA,
                                    reverse = F)
  # commercial viability palettes
  comm_var_pal  <- colorNumeric(turbo(n =20), domain = vect, na.color = NA,
                                reverse = F)

  reclass_pal   <- colorNumeric(turbo(n =20, direction = -1), domain = vect, na.color = NA,
                                reverse = F)

  sedim_pal     <- colorNumeric(turbo(n =20, direction = -1), domain = values(sediment_rate3), na.color = NA,
                                reverse = F)

  # rc_fetch_pal <- colorFactor(c("red", "green", "cyan"), values(rc_fetch),   na.color = "transparent")
  # rc_wlvl_pal <- colorFactor(c("red", "green", "cyan"), values(rc_wlvl), na.color = "transparent")
  # rc_road_pal <- colorFactor(c("red", "green", "cyan"), values(rc_road), na.color = "transparent")

  # rc_fetch_pal <- c("red", "green", "cyan")

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
    addRasterImage(
      sal3,
      opacity   = 0.8,
      colors    = sal_pal,
      group     = "2023 MP Mean annual salinity (Year 1)") %>%
    addLegend(
      pal       = sal_pal,
      title     = "2023 MP Mean annual salinity (Year 1)", position  = "bottomleft",
      labFormat = labelFormat(suffix = " g/L"),
      group     = "2023 MP Mean annual salinity (Year 1)",  layerId = "2023 MP Mean annual salinity (Year 1)", values = sal_cols$numeric_cols) %>%
    addRasterImage(
      wlvl_clamp,
      colors    = wlvl_pal,
      opacity   = 0.8,
      group     = "Water level variability (2021)") %>%
    addLegend(
      pal       = wlvl_pal,
      labFormat = labelFormat(suffix = " m"),
      title     = "WLV (2021)", position  = "bottomleft",
      group     = "Water level variability (2021)", layerId   = "Water level variability (2021)", values = values(wlvl_clamp)) %>%
    addRasterImage(
      depth1,
      opacity   = 0.8,
      colors    =  brewer.pal(9, "Spectral"),  # colors    =  c(brewer.pal(11, "Spectral")[c(2, 6)],"#5E4FA2"),
      group     = "2023 MP Depth (Year 1)") %>%
    addLegend(
      pal       = depth_fact_pal,
      title     = "2023 MP Depth (Year 1)",  position  = "bottomleft",
      group     = "2023 MP Depth (Year 1)",  layerId = "2023 MP Depth (Year 1)", values = depth_labels) %>%
  addRasterImage(
      sediment_rate3,
      project   = T,
      colors    = turbo(n =20, direction = -1),
      opacity   = 0.7,
      group     = "Sedimentation rate (Year 1)") %>%
  addLegend(
      pal       = sedim_pal,
      position  = "bottomleft",
      values    = values(sediment_rate3),
      title     = "Sedimentation rate (Year 1)",
      group     = "Sedimentation rate (Year 1)",  layerId   = "Sedimentation rate (Year 1)") %>%
    addRasterImage(
      rc_sediment3,
      project   = T,
      colors    = turbo(n =20, direction = -1),
      opacity   = 0.7,
      group     = "Sedimentation rate CV (Year 1)") %>%
    addLegend(
      pal       = reclass_pal,
      position  = "bottomleft", values    = vect,
      title     = "Sedimentation rate CV (Year 1)",
      group     = "Sedimentation rate CV (Year 1)",  layerId   = "Sedimentation rate CV (Year 1)") %>%
    addRasterImage(
      shallow_cv3,
      project     = T,
      colors      = viridisLite::turbo(n = 20),
      opacity     = 0.7,
      group       = "Shallow water CV (Year 1)") %>%
    addLegend(
      pal         = comm_var_pal,
      title       = "Shallow water CV (Year 1)", position  = "topleft",
      group       = "Shallow water CV (Year 1)", layerId = "Shallow water CV (Year 1)", values = vect) %>%
    addRasterImage(
        shallow_cv3_no_roads,
        project     = T,
        colors      = viridisLite::turbo(n = 20),
        opacity     = 0.7,
        group       = "Shallow water CV (Year 1 - No roads buffer)") %>%
    addLegend(
        pal         = comm_var_pal,
        title       = "Shallow water CV (Year 1 - No roads buffer)", position  = "topleft",
        group       = "Shallow water CV (Year 1 - No roads buffer)", layerId = "Shallow water CV (Year 1 - No roads buffer)", values = vect) %>%
    addRasterImage(
      deep_cv3,
      project     = T,
      colors      = viridisLite::turbo(n = 20),
      opacity     = 0.7,
      group       = "Deep water CV (Year 1)") %>%
    addLegend(
      pal         = comm_var_pal,
      title       = "Deep water CV (Year 1)", position  = "topleft",
      group       = "Deep water CV (Year 1)", layerId = "Deep water CV (Year 1)", values = vect) %>%
    addRasterImage(
      hsi_sal3,
      colors      = viridisLite::viridis(n = 20),
      opacity     = 0.7,
      group       = "2023 MP Oyster HSI (Year 1)") %>%
    addLegend(
      pal         = hsi_pal,
      title       = "2023 MP Oyster HSI (Year 1)", position  = "bottomleft",
      group       = "2023 MP Oyster HSI (Year 1)",
      layerId     = "2023 MP Oyster HSI (Year 1)",
      values      = values(hsi_sal3)) %>%
    addRasterImage(
      hsi_sal3_mask,
      colors      = viridisLite::viridis(n = 20),
      opacity     = 0.7,
      group       = "2023 MP Oyster HSI (Year 1) full mask") %>%
    addLegend(
      pal         = hsi_pal,
      title       = "2023 MP Oyster HSI (Year 1) full mask", position  = "bottomleft",
      group       = "2023 MP Oyster HSI (Year 1) full mask",
      layerId     = "2023 MP Oyster HSI (Year 1) full mask",
      values      = values(hsi_sal3)) %>%
    # addRasterImage(
    #   rc_fetch,
    #   project   = T,
    #   # colors    = viridis(n =20),
    #   colors    = turbo(n =20, direction = -1),  # colors = rc_fetch_pal,
    #   opacity   = 0.7,
    #   group     = "Fetch CV") %>%
    # addLegend(
    #   pal       = reclass_pal,
    #   position  = "bottomleft",
    #   # colors = rc_fetch_pal, values = unique(values(rc_fetch)),labels = na.omit(unique(values(rc_fetch))),
    #   values    = vect,
    #   title     = "Fetch CV",
    #   group     = "Fetch CV",  layerId   = "Fetch CV") %>%
    # addRasterImage(
    #   rc_wlvl,
    #   project   = T,
    #   # colors    = viridis(n =20),
    #   colors    = turbo(n =20, direction = -1),
    #   opacity   = 0.7,
    #   group     = "Water level variability CV") %>%
    # addLegend(
    #   pal       = reclass_pal,
    #   position  = "bottomleft", values    = vect,
    #   title     = "Water level variability CV",
    #   group     = "Water level variability CV",  layerId   = "Water level variability CV") %>%
    # addRasterImage(
    #   rc_road,
    #   project   = T,
    #   # colors    = viridis(n =20),
    #   colors    = turbo(n =20, direction = -1),
    #   opacity   = 0.7,
    #   group     = "Distance to Roads CV") %>%
    # addLegend(
    #   pal       = reclass_pal,
    #   position  = "bottomleft", values    = vect,
    #   title     = "Distance to Roads CV",
    #   group     = "Distance to Roads CV",  layerId   ="Distance to Roads CV") %>%
      # addPolygons(
      #   data               = land_sf,
      #   fillColor          = 'white', fillOpacity =  0.4,
      #   col                = "black", opacity  = 1, weight  = 1.5,
      #   group              = "2023 MP Land (Year 1)", label = ~label) %>%
      # addLegend(
      #   pal                = land_pal,
      #   position           = "topleft", values = land_label,
      #   group              = "2023 MP Land (Year 1)",
      #   layerId            = "2023 MP Land (Year 1)") %>%
      addLayersControl(
      options = layersControlOptions(collapsed = TRUE),
      baseGroups = c("Imagery", "Topographic", "Nat. Geo. Topographic"), overlayGroups = c(
        "2023 MP Mean annual salinity (Year 1)",
        "Water level variability (2021)",
        # "2023 MP Land (Year 1)",
        "2023 MP Depth (Year 1)",
        "Sedimentation rate (Year 1)",
        "Shallow water CV (Year 1)",
        "Shallow water CV (Year 1 - No roads buffer)",
        "Deep water CV (Year 1)",
        # "Fetch CV",
        # "Water level variability CV",
        # "Distance to Roads CV",
        "Sedimentation rate CV (Year 1)",
        "2023 MP Oyster HSI (Year 1)",
        "2023 MP Oyster HSI (Year 1) full mask")) %>%
    hideGroup(
      c(
        # "2023 MP Land (Year 1)"
        "Sedimentation rate (Year 1)",
        "Shallow water CV (Year 1)",
        "Shallow water CV (Year 1 - No roads buffer)",
        "Deep water CV (Year 1)",
        # "Fetch CV",
        # "Water level variability CV",
        # "Distance to Roads CV",
        "Sedimentation rate CV (Year 1)",
        "2023 MP Mean annual salinity (Year 1)",
        "2023 MP Depth (Year 1)",
        "Water level variability (2021)",
        # "Fetch",
        "2023 MP Oyster HSI (Year 1)",
        "2023 MP Oyster HSI (Year 1) full mask"
      ))
}

# --- Shiny utils ---
model_map2 <- function(
  # land_sf,
  sediment_rate10,
  shallow_cv10,
  deep_cv10,
  # rc_fetch,
  # rc_wlvl,
  # rc_road,
  rc_sediment10,
  sal10,
  depth1,
  wlvl_clamp,
  hsi_sal10,
  hsi_sal10_mask,
  pts = NULL
) {

  # ==================================
  # ---- COLOR PALETTES & LABELS -----
  # ==================================

  # Salinity color palette + labels
  sal_cols         <- data.frame(numeric_cols = 1:36)
  sal_pal          <- colorNumeric('viridis', domain = sal_cols$numeric_cols, na.color = NA, reverse = TRUE)

  # Water level variability palatte + labels
  wlvl_pal          <- colorNumeric(turbo(n = 20), domain = values(wlvl_clamp), na.color = NA, reverse = F)

  # Depth factors color palette + labels
  depth_lvl          <- data.frame(numeric_cols = 1:3)
  depth_labels       <- c("Too shallow", "Shallow water", "Deep water")
  depth_fact_pal     <- colorFactor(brewer.pal(9, "Spectral"), domain = depth_labels, reverse = T)


  # Land water legend color palette + label
  land_label        <- "2023 MP Land (Year 1)"
  land_pal          <- colorFactor(c("grey"),   domain = land_sf$label)

  # HSI 2017 500m grid color + labels
  vect <- 0:1
  hsi_pal           <- colorNumeric(viridis(n =20), domain = vect, na.color = NA,
                                    reverse = F)
  # commercial viability palettes
  comm_var_pal  <- colorNumeric(turbo(n =20), domain = vect, na.color = NA,
                                reverse = F)

  reclass_pal   <- colorNumeric(turbo(n =20, direction = -1), domain = vect, na.color = NA,
                                reverse = F)

  sedim_pal     <- colorNumeric(turbo(n =20, direction = 1), domain = values(sediment_rate10), na.color = NA,
                                reverse = F)

  # rc_fetch_pal <- colorFactor(c("red", "green", "cyan"), values(rc_fetch),   na.color = "transparent")
  # rc_wlvl_pal <- colorFactor(c("red", "green", "cyan"), values(rc_wlvl), na.color = "transparent")
  # rc_road_pal <- colorFactor(c("red", "green", "cyan"), values(rc_road), na.color = "transparent")

  # rc_fetch_pal <- c("red", "green", "cyan")

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

    addRasterImage(
      sal10,
      opacity   = 0.8,
      colors    = sal_pal,
      group     = "2023 MP Mean annual salinity (Year 8)") %>%
    addLegend(
      pal       = sal_pal,
      title     = "2023 MP Mean annual salinity (Year 8)", position  = "bottomleft",
      labFormat = labelFormat(suffix = " g/L"),
      group     = "2023 MP Mean annual salinity (Year 8)",  layerId = "2023 MP Mean annual salinity (Year 8)", values = sal_cols$numeric_cols) %>%
    addRasterImage(
      wlvl_clamp,
      colors    = wlvl_pal,
      opacity   = 0.8,
      group     = "Water level variability (2021)") %>%
    addLegend(
      pal       = wlvl_pal,
      labFormat = labelFormat(suffix = " m"),
      title     = "WLV (2021)", position  = "bottomleft",
      group     = "Water level variability (2021)", layerId   = "Water level variability (2021)", values = values(wlvl_clamp)) %>%
    addRasterImage(
      depth1,
      opacity   = 0.8,
      colors    =  brewer.pal(9, "Spectral"),  # colors    =  c(brewer.pal(11, "Spectral")[c(2, 6)],"#5E4FA2"),
      group     = "2023 MP Depth (Year 1)") %>%
    addLegend(
      pal       = depth_fact_pal,
      title     = "2023 MP Depth (Year 1)",  position  = "bottomleft",
      group     = "2023 MP Depth (Year 1)",  layerId = "2023 MP Depth (Year 1)", values = depth_labels) %>%
    addRasterImage(
      sediment_rate10,
      project   = T,
      colors    = turbo(n =20, direction = 1),
      opacity   = 0.7,
      group     = "Sedimentation rate (Year 8)") %>%
    addLegend(
      pal       = sedim_pal,
      position  = "bottomleft",
      values    = values(sediment_rate10),
      title     = "Sedimentation rate (Year 8)",
      group     = "Sedimentation rate (Year 8)",  layerId   = "Sedimentation rate (Year 8)") %>%
    # addRasterImage(
    #   rc_fetch,
    #   project   = T,
    #   # colors    = viridis(n =20),
    #   colors    = turbo(n =20, direction = -1),  # colors = rc_fetch_pal,
    #   opacity   = 0.7,
    #   group     = "Fetch CV") %>%
    # addLegend(
    #   pal       = reclass_pal,
    #   position  = "bottomleft",
    #   # colors = rc_fetch_pal, values = unique(values(rc_fetch)),labels = na.omit(unique(values(rc_fetch))),
    #   values    = vect,
    #   title     = "Fetch CV",
    #   group     = "Fetch CV",  layerId   = "Fetch CV") %>%
    # addRasterImage(
    #   rc_wlvl,
    #   project   = T,
    #   # colors    = viridis(n =20),
    #   colors    = turbo(n =20, direction = -1),
    #   opacity   = 0.7,
    #   group     = "Water level variability CV") %>%
    # addLegend(
    #   pal       = reclass_pal,
    #   position  = "bottomleft", values    = vect,
    #   title     = "Water level variability CV",
    #   group     = "Water level variability CV",  layerId   = "Water level variability CV") %>%
    # addRasterImage(
    #   rc_road,
    #   project   = T,
    #   # colors    = viridis(n =20),
    #   colors    = turbo(n =20, direction = -1),
    #   opacity   = 0.7,
    #   group     = "Distance to Roads CV") %>%
    # addLegend(
    #   pal       = reclass_pal,
    #   position  = "bottomleft", values    = vect,
    #   title     = "Distance to Roads CV",
    #   group     = "Distance to Roads CV",  layerId   ="Distance to Roads CV") %>%
    addRasterImage(
      rc_sediment10,
      project   = T,
      colors    = turbo(n =20, direction = -1),
      opacity   = 0.7,
      group     = "Sedimentation rate CV (Year 8)") %>%
    addLegend(
      pal       = reclass_pal,
      position  = "bottomleft", values    = vect,
      title     = "Sedimentation rate CV (Year 8)",
      group     = "Sedimentation rate CV (Year 8)",  layerId   = "Sedimentation rate CV (Year 8)") %>%
    addRasterImage(
      shallow_cv10,
      project     = T,
      colors      = viridisLite::turbo(n = 20),
      opacity     = 0.7,
      group       = "Shallow water CV (Year 8)") %>%
    addLegend(
      pal         = comm_var_pal,
      title       = "Shallow water CV (Year 8)", position  = "topleft",
      group       = "Shallow water CV (Year 8)", layerId = "Shallow water CV (Year 8)", values = vect) %>%
    addRasterImage(
      deep_cv10,
      project     = T,
      colors      = viridisLite::turbo(n = 20),
      opacity     = 0.7,
      group       = "Deep water CV (Year 8)") %>%
    addLegend(
      pal         = comm_var_pal,
      title       = "Deep water CV (Year 8)", position  = "topleft",
      group       = "Deep water CV (Year 8)", layerId = "Deep water CV (Year 8)", values = vect) %>%
    addRasterImage(
      hsi_sal10,
      colors    = viridisLite::viridis(n = 20),
      opacity   = 0.7,
      group     = "2023 MP Oyster HSI (Year 8)") %>%
    addLegend(
      pal       = hsi_pal,
      title     = "2023 MP Oyster HSI (Year 8)", position  = "bottomleft",
      group     = "2023 MP Oyster HSI (Year 8)", layerId = "2023 MP Oyster HSI (Year 8)", values = values(hsi_sal10)) %>%
    addRasterImage(
      hsi_sal10_mask,
      colors    = viridisLite::viridis(n = 20),
      opacity   = 0.7,
      group     = "2023 MP Oyster HSI (Year 8) full mask") %>%
    addLegend(
      pal       = hsi_pal,
      title     = "2023 MP Oyster HSI (Year 8) full mask", position  = "bottomleft",
      group     = "2023 MP Oyster HSI (Year 8) full mask", layerId = "2023 MP Oyster HSI (Year 8) full mask", values = values(hsi_sal10)) %>%
    # addPolygons(
    #   data               = land_sf,
    #   fillColor          = 'white', fillOpacity =  0.4,
    #   col                = "black", opacity  = 1, weight  = 1.5,
    #   group              = "2023 MP Land (Year 1)", label = ~label) %>%
    # addLegend(
    #   pal                = land_pal,
    #   position           = "topleft", values = land_label,
    #   group              = "2023 MP Land (Year 1)",
    #   layerId            = "2023 MP Land (Year 1)") %>%
    addLayersControl(
      options = layersControlOptions(collapsed = TRUE),
      baseGroups = c("Imagery", "Topographic", "Nat. Geo. Topographic"), overlayGroups = c(
        "2023 MP Mean annual salinity (Year 8)",
        "Water level variability (2021)",
        # "2023 MP Land (Year 1)",
        "2023 MP Depth (Year 1)",
        "Sedimentation rate (Year 8)",
        "Shallow water CV (Year 8)",
        "Deep water CV (Year 8)",
        # "Fetch CV",
        # "Water level variability CV",
        # "Distance to Roads CV",
        "Sedimentation rate CV (Year 8)",
        "2023 MP Oyster HSI (Year 8)",
        "2023 MP Oyster HSI (Year 8) full mask")) %>%
    hideGroup(
      c(
        # "2023 MP Land (Year 1)"
        "Sedimentation rate (Year 8)",
        "Shallow water CV (Year 8)",
        "Deep water CV (Year 8)",
        # "Fetch CV",
        # "Water level variability CV",
        # "Distance to Roads CV",
        "Sedimentation rate CV (Year 8)",
        "2023 MP Mean annual salinity (Year 8)",
        "2023 MP Depth (Year 1)",
        "Water level variability (2021)",
        "Fetch",
        "2023 MP Oyster HSI (Year 8)",
        "2023 MP Oyster HSI (Year 8) full mask"
        )
    )

}
