library(tidyverse)
library(fs)
library(readxl)

path_sharepoint <- "~/MSF/EpiDS - GTFCC-OCV/data-clean"
path_data <- max(dir_ls(path_sharepoint, glob = "*.xlsx"))
dat <- excel_sheets(path_data) %>% purrr::set_names() %>% map(~read_excel(path_data, .x))
dat$request <- dat$request %>% 
  mutate(
    request_country = recode(request_country, "Zanzibar" = "Tanzania"),
    iso_a3 = countrycode::countrycode(request_country, origin = "country.name", destination = "iso3c"),
    .after = request_country
  ) %>% 
  mutate(
    quarter = lubridate::quarter(date_receipt, with_year = TRUE) %>% str_replace("\\.", "-Q")
  ) %>% 
  left_join(
    dat$shipment %>% 
      group_by(id_demand) %>% 
      summarise(n_dose_ship = sum(n_dose_ship, na.rm = TRUE), .groups = "drop"), 
    by = "id_demand"
  )

data_dir <- here::here("data")
if (!dir_exists(data_dir)) dir_create(data_dir)

walk2(dat, names(dat), ~{
  write_rds(.x, path(data_dir, .y, ext = "rds"))
})

if (FALSE) {
  library(summarytools)
  walk2(dat, names(dat), ~{
    df_sum <- dfSummary(.x)
    view(df_sum, method = "browser", file = path("data-raw", .y, ext = "html"))
  })
}