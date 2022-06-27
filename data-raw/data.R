library(tidyverse)
library(fs)
library(readxl)
source(here::here("R", "utils_data.R"))


path_sharepoint <- "~/MSF/EpiDS - GTFCC-OCV/data-clean"
path_data <- max(dir_ls(path_sharepoint, glob = "*.xlsx"))

date_updated <- fs::file_info(path_data)$modification_time %>% lubridate::as_date()
write_rds(date_updated, here::here("data", "date_updated.rds"))

dat <- excel_sheets(path_data) %>% purrr::set_names() %>% map(~qxl::qread(path_data, .x))
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

df_vaccine <- dat$shipment %>% 
  distinct(id_demand, vaccine) %>% 
  group_by(id_demand) %>% 
  summarise(vaccine = paste(vaccine, collapse = ", "), .groups = "drop")

df_info <- dat$request %>% 
  distinct(id_demand, who_region, request_country, request_status, context, request_mechanism, request_agency) %>% 
  left_join(df_vaccine, by = "id_demand")

df_timeline <- get_timevis_df(dat$request, dat$shipment, dat$round)

df_timevis <- df_info %>% 
  left_join(df_timeline, by = "id_demand") %>% 
  drop_na(start) %>% 
  mutate(
    type = "point", 
    title = glue::glue("ID: {id_demand} | Date: {format(start, '%d %b %y')}")
  ) %>% 
  rename(group = request_country)

df_delay <- df_info %>% 
  left_join(get_delay_df(df_timeline), by = "id_demand") %>% 
  drop_na(date)

df_event_max <- df_delay %>% 
  separate(event, c("event", "index"), sep = "_") %>% 
  group_by(event) %>% 
  summarise(max = max(index, na.rm = TRUE), .groups = "drop")

max_shipments <- df_event_max %>% filter(event == "shipment") %>% pull(max) %>% as.numeric()
max_rounds <- df_event_max %>% filter(event == "round") %>% pull(max) %>% as.numeric()

app_data <- c(
  dat,
  tibble::lst(
    df_info,
    df_timevis,
    df_delay,
    max_shipments,
    max_rounds
  )
)

data_dir <- here::here("data")
if (!dir_exists(data_dir)) dir_create(data_dir)

write_rds(app_data, path(data_dir, "app_data", ext = "rds"))

# walk2(dat, names(dat), ~{
#   write_rds(.x, path(data_dir, .y, ext = "rds"))
# })

if (FALSE) {
  library(summarytools)
  walk2(dat, names(dat), ~{
    df_sum <- dfSummary(.x)
    view(df_sum, method = "browser", file = path("data-raw", .y, ext = "html"))
  })
}

