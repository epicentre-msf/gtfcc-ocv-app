library(tidyverse)
library(lubridate)
library(shinylogs)

read_session_logs <- function(dir) {
  df_session <- tibble::as_tibble(
    shinylogs::read_json_logs(dir)$session
  )
}

gtfcc_logs <- read_session_logs("/home/epicentre/gtfcc-ocv-app/logs") %>%
  rename(username = user) %>%
  mutate(
    username = NA_character_,
    date = as_date(server_connected),
    week = floor_date(date, "week", 1),
    month = floor_date(date, "month"),
    .after = sessionid
  )

file_out <- fs::path(tempdir(), "gtfcc-usage.rds")

write_rds(gtfcc_logs, file_out)

system(
  glue::glue("rsync -zavh {file_out} epicentre@vps709766.ovh.net:/srv/shiny-server/usage-stats/data/")
)
