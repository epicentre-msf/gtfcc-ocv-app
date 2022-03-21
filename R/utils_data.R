
filter_geo <- function(df, geo_select) {
  # if (geo_select == "World") return(df)
  who_regions <- c("AMRO", "AFRO", "SEARO", "EMRO", "WPRO", "EURO")
  region_select <- geo_select[geo_select %in% who_regions]
  country_select <- setdiff(geo_select, who_regions)
  rl <- length(region_select)
  cl <- length(country_select)
  if (rl & cl) {
    df %<>% filter(who_region %in% region_select | request_country %in% country_select)
  } else if (rl) {
    df %<>% filter(who_region %in% region_select)
  } else if (cl) {
    df %<>% filter(request_country %in% country_select)
  }
  return(df)
}