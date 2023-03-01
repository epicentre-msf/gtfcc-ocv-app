

df_demands <- dat$request %>%
  mutate(
    r_country = recode(r_country, "Zanzibar" = "Tanzania"),
    iso_a3 = countrycode::countrycode(r_country, origin = "country.name", destination = "iso3c"),
    .after = r_country
  ) %>%
  distinct(r_demand_id, iso_a3, r_country)

df_counts <- dat$shipment %>%
  filter(s_year_delivery == 2022) %>% 
  group_by(s_r_demand_id, s_year_delivery) %>%
  summarise(s_dose_ship = sum(s_dose_ship, na.rm = TRUE), .groups = "drop") %>% 
  left_join(df_demands, by = c("s_r_demand_id" = "r_demand_id")) %>% 
  count(iso_a3, wt = s_dose_ship)

df_map <- sf_world %>%
  sf::st_drop_geometry() %>%
  select(country, iso_a3, lon, lat) %>%
  left_join(df_counts, by = "iso_a3") %>%
  mutate(across(where(is.numeric), as.double)) %>%
  mutate(across(where(is.double), ~ if_else(is.na(.x), 0, .x)))

chartData <- df_map %>% select(any_of("n"))
# select(-country, -iso_a3, -lon, -lat, -total)
pie_width <- 45 * sqrt(df_map$n) / sqrt(max(df_map$n))


library(leaflet)
library(leaflet.minicharts)

leaflet::leaflet() %>%
  leaflet::setView(lng = 11, lat = 4.64916, zoom = 3) %>%
  # leaflet::fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]]) %>%
  leaflet::addMapPane(name = "choropleth", zIndex = 300) %>%
  leaflet::addMapPane(name = "circles", zIndex = 420) %>%
  leaflet::addMapPane(name = "geo_highlight", zIndex = 430) %>%
  leaflet::addMapPane(name = "place_labels", zIndex = 440) %>%
  # leaflet::addProviderTiles("CartoDB.PositronNoLabels", group = "Light") %>%
  # leaflet::addProviderTiles("CartoDB.PositronOnlyLabels", group = "Labels", options = leaflet::leafletOptions(pane = "place_labels")) %>%
  leaflet::addProviderTiles("OpenStreetMap", group = "OSM") %>%
  # leaflet::addProviderTiles("OpenStreetMap.HOT", group = "OSM HOT") %>%
  leaflet::addScaleBar(position = "bottomleft") %>%
  leaflet.extras::addFullscreenControl(position = "topleft") %>%
  leaflet.extras::addResetMapButton() %>%
  # leaflet::addLayersControl(
  #   # baseGroups = c("OSM", "OSM HOT", "Light"),
  #   overlayGroups = c("Labels"),
  #   position = "topleft"
  # ) %>%
  leaflet::addPolygons(
    data = sf_world,
    stroke = TRUE,
    color = "grey",
    weight = 1,
    fillOpacity = 0,
    label = ~country,
    layerId = ~iso_a3,
    group = "Boundaries",
    options = leaflet::pathOptions(pane = "choropleth")
  ) %>%
  addMinicharts(
    sf_world$lon,
    sf_world$lat,
    layerId = df_map$country,
    chartdata = chartData,
    width = pie_width,
    opacity = .7,
    legend = TRUE,
    showLabels = TRUE,
    type = "pie"
  )

RUN R -e 'remotes::install_cran("shinyWidgets")'
RUN R -e 'remotes::install_cran("shinydashboard")'
RUN R -e 'remotes::install_cran("shinyjs")'
RUN R -e 'remotes::install_cran("aweek")'
RUN R -e 'remotes::install_cran("janitor")'
RUN R -e 'remotes::install_cran("gtsummary")'
RUN R -e 'remotes::install_github("paulc91/leaflet")'
RUN R -e 'remotes::install_cran("leaflet.extras")'
RUN R -e 'remotes::install_github("JohnCoene/countup")'
RUN R -e 'remotes::install_github("JohnCoene/pushbar")'
RUN R -e 'remotes::install_github("JohnCoene/waiter")'
RUN R -e 'remotes::install_github("dreamRs/capture")'
RUN R -e 'remotes::install_github("epicentre-msf/qxl")'
RUN R -e 'remotes::install_cran("shiny.i18n")'
RUN R -e 'remotes::install_github("dreamRs/shinylogs")'
RUN R -e 'remotes::install_github("JohnCoene/cicerone")'
RUN R -e 'remotes::install_cran("leaflet.minicharts")'
RUN R -e 'remotes::install_github("JohnCoene/sever")'
RUN R -e 'remotes::install_github("JohnCoene/tippy")'
RUN R -e 'remotes::install_github("glin/reactable")'
RUN R -e 'install.packages("highcharter")'
RUN R -e 'remotes::install_cran("thematic")'
RUN R -e 'remotes::install_cran("showtext")'
RUN R -e 'remotes::install_cran("lwgeom")'

cran_pkgs <- c(
  "shinyWidgets",
  "shinydashboard",
  "shinyjs",
  "aweek",
  "gtsummary",
  "shiny.i18n",
  "highcharter",
  "thematic",
  "showtext",
  "lwgeom",
  "rmapshaper",
  "ggrepel",
  "ggtext",
  "scico",
  "patchwork",
  "binom",
  "shinylogs",
  "leaflet.minicharts",
  "leaflet.extras"
)

github_pkgs <- c(
  "rstudio/thematic",
  "glin/reactable",
  "paulc91/leaflet",
  "dreamRs/capture"
)

coene_pkgs <- paste0("JohnCoene/", c("countup", "pushbar", "waiter", "cicerone", "sever", "tippy"))
