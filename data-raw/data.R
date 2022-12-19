library(tidyverse)
library(fs)
library(readxl)
source(here::here("R", "utils_data.R"))

path_sharepoint <- "~/MSF/EpiDS - GTFCC-OCV/data-clean/export4dashboard/"
path_data <- max(dir_ls(path_sharepoint, regexp = "gtfcc_ocv_data_dashoard__.*.xlsx"))

date_updated <- fs::file_info(path_data)$modification_time %>% lubridate::as_date()
write_rds(date_updated, here::here("data", "date_updated.rds"))

data_sheets <- c("Request", "Shipment", "Campaign and round")

dat <- data_sheets %>% 
  purrr::set_names(snakecase::to_snake_case(.)) %>% 
  map(~qxl::qread(path_data, .x))

dat$request <- dat$request %>% 
  mutate(
    r_country = recode(r_country, "Zanzibar" = "Tanzania"),
    iso_a3 = countrycode::countrycode(r_country, origin = "country.name", destination = "iso3c"),
    .after = r_country
  ) %>% 
  mutate(
    quarter = lubridate::quarter(r_date_receipt, with_year = TRUE) %>% str_replace("\\.", "-Q")
  ) %>% 
  left_join(
    dat$shipment %>% 
      group_by(s_r_demand_id) %>% 
      summarise(s_dose_ship = sum(s_dose_ship, na.rm = TRUE), .groups = "drop"), 
    by = c("r_demand_id" = "s_r_demand_id")
  )

df_vaccine <- dat$shipment %>% 
  distinct(s_r_demand_id, s_vaccine) %>% 
  group_by(s_r_demand_id) %>% 
  summarise(s_vaccine = paste(s_vaccine, collapse = ", "), .groups = "drop")

df_info <- dat$request %>% 
  distinct(
    r_demand_id,
    r_who_region,
    r_country,
    r_status,
    r_context,
    r_mechanism,
    r_agency
  ) %>% 
  left_join(df_vaccine, by = c("r_demand_id" = "s_r_demand_id"))

df_timeline <- get_timevis_df(dat$request, dat$shipment, dat$campaign_and_round)

df_timevis <- df_info %>% 
  left_join(df_timeline, by = "r_demand_id") %>% 
  drop_na(start) %>% 
  mutate(
    type = "point", 
    title = glue::glue("ID: {r_demand_id} | Date: {format(start, '%d %b %y')}")
  ) %>% 
  rename(group = r_country)

df_delay <- df_info %>% 
  left_join(get_delay_df(df_timeline), by = "r_demand_id") %>% 
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

if (FALSE) {
  library(summarytools)
  walk2(dat, names(dat), ~{
    df_sum <- dfSummary(.x)
    view(df_sum, method = "browser", file = path("data-raw", .y, ext = "html"))
  })
}

