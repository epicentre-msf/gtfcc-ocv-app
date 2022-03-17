# make a scaling function to convert real numbers to radii appropriate for leaflet
calc_radius <- function(n, scale_factor = 30) {
  sqrt(n)/sqrt(max(n)) * scale_factor
}

get_higher_level_pcode <- function(x) {
  stringr::str_extract(x, ".*(?=__)")
}

make_tooltip <- function(df, adm_lvl) {
  ar <- ifelse(is.na(df$attack_rate), "0", round(df$attack_rate, 1))
  glue::glue(
    "<b>{df$name}</b><br>
       Cas: <b>{scales::number(df$cas, accuracy = 1)}</b><br>
       Décès: <b>{df$deces}</b><br>
       Taux d'attaque: <b>{ar}</b><br>"
  ) %>% purrr::map(htmltools::HTML)
}

make_tooltip_lab <- function(df) {
  t <- ifelse(is.na(df$total), "0", df$total)
  glue::glue(
    "<b>{df$name}</b><br>
       Tests: <b>{scales::number(t, accuracy = 1)}</b><br>"
  ) %>% purrr::map(htmltools::HTML)
}

tag_map_title <- tags$style(HTML("
  .leaflet-control.map-title {
    display: block;
    margin: auto;
    text-align: center;
    padding-left: 10px;
    padding-right: 10px;
    color: rgba(85, 85, 85);
    font-size: 18px;
    background: rgba(255,255,255,0.8);
    box-shadow: 0 0 15px rgba(0,0,0,0.2);
    border-radius: 5px;
  }
"))

tag_map_info <- shiny::tags$style(shiny::HTML("
  .leaflet-control.map-title {
    text-align: center;
    padding-left: 10px;
    padding-right: 10px;
    color: rgba(85, 85, 85);
    font-size: 12px;
    font-family: Arial;
    background: rgba(255,255,255,0.8);
    box-shadow: 0 0 15px rgba(0,0,0,0.2);
    border-radius: 5px;
  }
"))

addCircleLegend <- function(
  map, title = "", range, scaling_fun, ...,
  color, weight, fillColor, fillOpacity,
  position = c("topright", "bottomright", "bottomleft", "topleft"),
  data = leaflet::getMapData(map), layerId = NULL, group = NULL) {

  circle_style <- glue::glue(
    "border-radius:50%;
    border: {weight}px solid {color};
    background: {paste0(fillColor, round(fillOpacity*100, 0))};
    position: absolute;
    bottom:1px;
    right:25%;
    left:50%;"
  )

  text_style <- glue::glue(
    "text-align: right;
    font-size: 11px;
    position: absolute;
    bottom: 0px;
    right:1px;"
  )

  if (length(range) == 1) {
    radii <- scaling_fun(range, ...)
    n_range <- scales::label_number_si()(range)

    circle_legend <- htmltools::HTML(glue::glue(
      '<div class="bubble-legend">
    <div id="legendTitle" style="text-align: center; font-weight: bold;">{title}</div>
    <div class="symbolsContainer" style="min-width: {radii[1]*2 + 20}px; min-height: {radii[1]*2}px;">
    <div class="legendCircle" style="width: {radii[1] * 2}px; height: {radii[1] * 2}px; margin-left: {-radii[1]}px; {circle_style}"></div>
    <div><p class="legendValue" style="margin-bottom: {radii[1] * 2 - 12}px; {text_style}">{n_range[1]}</p></div>
    </div>
    </div>'
    ))
  } else if (length(range) == 2) {
    radii <- sort(scaling_fun(range, ...))
    n_range <- scales::label_number_si()(sort(range))

    circle_legend <- htmltools::HTML(glue::glue(
      '<div class="bubble-legend">
    <div id="legendTitle" style="text-align: center; font-weight: bold;">{title}</div>
    <div class="symbolsContainer" style="min-width: {radii[2]*2 + 20}px; min-height: {radii[2]*2}px;">
    <div class="legendCircle" style="width: {radii[2] * 2}px; height: {radii[2] * 2}px; margin-left: {-radii[2]}px; {circle_style}"></div>
    <div class="legendCircle" style="width: {radii[1] * 2}px; height: {radii[1] * 2}px; margin-left: {-radii[1]}px; {circle_style}"></div>
    <div><p class="legendValue" style="margin-bottom: {radii[1] * 2 - 12}px; {text_style}">{n_range[1]}</p></div>
    <div><p class="legendValue" style="margin-bottom: {radii[2] * 2 - 12}px; {text_style}">{n_range[2]}</p></div>
    </div>
    </div>'
    ))
  } else {
    range <- base::pretty(sort(range), 20)
    range <- range[range != 0]
    min_n <- ceiling(min(range, na.rm = TRUE))
    med_n <- round(median(range, na.rm = TRUE), 0)
    max_n <- round(max(range, na.rm = TRUE), 0)
    n_range <- c(min_n, med_n, max_n)
    radii <- scaling_fun(n_range, ...)
    n_range <- scales::label_number_si()(n_range)

    circle_legend <- htmltools::HTML(glue::glue(
      '<div class="bubble-legend">
    <div id="legendTitle" style="text-align: center; font-weight: bold;">{title}</div>
    <div class="symbolsContainer" style="min-width: {radii[3]*2 + 20}px; min-height: {radii[3]*2}px;">
    <div class="legendCircle" style="width: {radii[3] * 2}px; height: {radii[3] * 2}px; margin-left: {-radii[3]}px; {circle_style}"></div>
    <div class="legendCircle" style="width: {radii[2] * 2}px; height: {radii[2] * 2}px; margin-left: {-radii[2]}px; {circle_style}"></div>
    <div class="legendCircle" style="width: {radii[1] * 2}px; height: {radii[1] * 2}px; margin-left: {-radii[1]}px; {circle_style}"></div>
    <div>
    <p class="legendValue" style="margin-bottom: {radii[1] * 2 - 12}px; {text_style}">{n_range[1]}</p>
    </div>
    <div>
    <p class="legendValue" style="margin-bottom: {radii[2] * 2 - 12}px; {text_style}">{n_range[2]}</p>
    </div>
    <div>
    <p class="legendValue" style="margin-bottom: {radii[3] * 2 - 12}px; {text_style}">{n_range[3]}</p>
    </div>
    </div>
    </div>'
    ))
  }

  return(
    #addControl(map, html = circle_legend, position = position, layerId = layerId)
    leaflet::addCustomLegend(map, html = circle_legend, position = position, layerId = layerId, group = group)
  )
}
