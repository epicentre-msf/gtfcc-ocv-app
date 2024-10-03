set_paths <- function(info = Sys.info()) {
  
  # get onedrive path from .Renviron
  paths <- list(
    onedrive = Sys.getenv("SHAREPOINT_PATH")
  )
  
  if (paths$onedrive == "") {
    stop("Missing SHAREPOINT_PATH variable in .Renviron. See ?usethis::edit_r_environ()")
  }
  
  # is server
  paths$is_server <- ifelse(info[["nodename"]] == "ip-10-0-3-151", TRUE, FALSE)
  
  # geo 
  paths$obtools_geo <- file.path(paths$onedrive, "OutbreakTools - GeoBase")

  # /proj/
  paths$proj       <- file.path(paths$onedrive, "EpiDS - GTFCC-OCV")
  paths$cleaning   <- file.path(paths$proj, "data-cleaning", "country")
  
  # /proj/data-cleaning/
  paths$dictionaries <- file.path(paths$cleaning, "dictionaries")
  paths$corrections  <- file.path(paths$cleaning, "corrections")
  paths$compilation  <- file.path(paths$cleaning, "compilation")
  
  paths$cleaning_dates    <- file.path(paths$corrections, "dates")
  paths$cleaning_geocodes <- file.path(paths$corrections, "geocodes")

  ## create dirs that don't yet exist ------------------------------------------
  if (!dir.exists(paths$cleaning)) dir.create(paths$cleaning, recursive = TRUE)
  
  if (!dir.exists(paths$dictionaries)) dir.create(paths$dictionaries)
  if (!dir.exists(paths$corrections)) dir.create(paths$corrections)
  if (!dir.exists(paths$compilation)) dir.create(paths$compilation)
  
  if (!dir.exists(paths$cleaning_dates)) dir.create(paths$cleaning_dates)
  if (!dir.exists(paths$cleaning_geocodes)) dir.create(paths$cleaning_geocodes)

  return(paths)
}

#' Convenience function for creating time stamps
time_stamp <- function(t = Sys.time(), format = "%Y-%m-%d_%H%M%S") {
  format(t, format = format)
}

import_country_data <- function(path, sheet = "Target areas") {
  qxl::qread(path, sheet) |> # , col_types = c_types
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("adm"), as.character),
      dplyr::across(dplyr::contains("pcode"), as.character),
      dplyr::across(dplyr::contains("nrow"), as.numeric),
      dplyr::across(dplyr::contains("population"), as.numeric),
      dplyr::across(dplyr::contains("dose"), as.numeric),
      dplyr::across(dplyr::contains("cov"), as.numeric),
      dplyr::across(dplyr::contains("drop"), as.numeric),
      dplyr::across(dplyr::contains("date"), as_date),
      adm1_t_target_area = stringr::str_remove(adm1_t_target_area, "^[A-Z]{3}(?=\\s)")
    )
}


#' Implement linelist geocoding routines
#'
#' @param dat Data frame containing raw admin columns
#' @param paths List of relevant paths
#' @param write_checks Logical indicating whether check files should be written
#'   (defaults to \code{TRUE})
#'
#' @return
#' Linelist with cleaned admin columns appended
#'
clean_geo <- function(dat, paths, write_checks = TRUE) {

  # when running manually ------------------------------------------------------
  if (FALSE) {
    dat <- ll_std
    write_checks <- FALSE
  }


  ## relevant cols -------------------------------------------------------------
  adm_cols_orig <- c("t_r_iso3", paste0("adm", 1:4, "_t_target_area"))
  adm_cols <- paste0("adm", 0:4, "_name")
  adm_cols_ref <- paste0("ref_", adm_cols)
  adm_cols_recode <- setNames(adm_cols_orig, adm_cols)


  ## assemble raw geo data -----------------------------------------------------
  dat_prep <- dat %>%
    mutate(
      # adm0_residence = str_extract(adm1_residence, "^[A-Z]{3}(?=\\s)"),
      # adm0_residence = if_else(adm0_residence %in% "Congo - Kinshasa", "Democratic Republic of the Congo", adm0_residence),
      t_r_iso3 = countrycode::countrycode(t_r_country, destination = "iso3c", origin = "country.name"),
      .before = t_r_country,
      across(all_of(adm_cols_orig), stringr::str_trim),
      across(all_of(adm_cols_orig), ~ na_if(.x, "N/A")),
      across(all_of(adm_cols_orig), ~ na_if(.x, "0"))
    )

  geo_raw <- dat_prep %>%
    select(!!!all_of(adm_cols_recode)) %>%
    filter(!if_all(everything(), is.na)) %>%
    count(across(everything()))


  ## assemble ref geo data -----------------------------------------------------
  adm0_levels <- sort(unique(dat_prep$t_r_iso3))
  if (paths$is_server) {
    sync_from_sharepoint_geo(paths, countries = adm0_levels)
  }

  geo_ref <- map(adm0_levels, ~ obtdata::fetch_georef(.x, path_onedrive = paths$onedrive)$ref) %>%
    list_rbind() %>%
    # mutate(adm0_name = countrycode::countrycode(adm0_name, origin = "country.name", destination = "iso3c")) %>% 
    # mutate(adm0_name = if_else(grepl("Yemen", adm0_name), "Yemen", adm0_name)) %>% # drop Arabic component from adm0 for YEM
    # mutate(adm1_name = paste(adm0_iso3, adm1_name)) %>%
    mutate(adm0_name = adm0_iso3) |> 
    select(level, all_of(adm_cols), pcode) %>%
    distinct(adm0_name, pcode, .keep_all = TRUE)


  # manual corrections --------------------------------------------------------
  # corr_geo <- qxl::qread(file.path(paths$corrections, "corr_geo.xlsx"), trim_ws = FALSE)

  geo_man_full <- list.files(paths$cleaning_geocodes, pattern = "^geocodes_check_", full.names = TRUE) %>%
    max() %>%
    readxl::read_xlsx(col_types = "text", trim_ws = FALSE) %>%
    distinct(across(all_of(c(adm_cols, "pcode_new"))), .keep_all = TRUE)

  geo_man <- geo_man_full %>%
    filter(!is.na(pcode_new)) %>%
    select(all_of(adm_cols), pcode = pcode_new)


  # geomatching ---------------------------------------------------------------
  df_match <- geo_raw %>%
    hmatch::hmatch_composite(
      raw = .,
      ref = geo_ref,
      man = geo_man,
      # dict = corr_geo,
      by = adm_cols,
      fuzzy = TRUE,
      type = "resolve_left",
      code_col = "pcode"
    ) %>%
    mutate(
      level_raw = hmatch::max_levels(., by = adm_cols),
      level_ref = hmatch::max_levels(., by = adm_cols_ref)
    )


  ## write unmatched levels to file --------------------------------------------
  geo_man_join <- geo_man_full %>%
    # update count
    select(!n) %>%
    inner_join(geo_raw, by = adm_cols) %>%
    relocate(n, .after = adm4_name) %>%
    mutate(
      across(c(level_raw, level_ref), as.integer),
      new = as.logical(NA)
    )

  df_check <- df_match %>%
    select(-level) %>%
    filter(level_ref < level_raw) %>%
    mutate(
      pcode_new = NA_character_,
      comment = NA_character_
    ) %>%
    anti_join(geo_man_join, by = adm_cols) %>%
    mutate(new = TRUE, .before = comment) %>%
    bind_rows(geo_man_join) %>%
    arrange(across(all_of(adm_cols)))

  if (any(df_check$new %in% TRUE) & write_checks) {
    # output paths
    path_geocodes_check <- file.path(paths$cleaning_geocodes, glue::glue("geocodes_check_{time_stamp()}.xlsx"))

    # write new geocodes_check file
    qxl::qxl(
      df_check,
      path_geocodes_check,
      style1 = qxl::qstyle(halign = "left"),
      protect = qxl::qprotect(cols = !pcode_new:comment),
      filter = TRUE,
      col_widths = c(new = 5, comment = 35)
    )

    message(
      glue::glue("{nrow(df_check)} unmatched geocodes ({sum(df_check$new %in% TRUE)} are new)")
    )

    # if server, copy files to sharepoint
    if (paths$is_server) {
      rclone_copy(path_geocodes_check, paths$rclone_cleaning_geocodes)
    }
  }


  ## bind matched admin levels and pcodes --------------------------------------
  geo_ref1 <- geo_ref %>%
    filter(level == 1) %>%
    select(adm0_name, adm1_name, ref_adm1_pcode = pcode)
  geo_ref2 <- geo_ref %>%
    filter(level == 2) %>%
    select(adm1_name, adm2_name, ref_adm2_pcode = pcode)
  geo_ref3 <- geo_ref %>%
    filter(level == 3) %>%
    select(adm1_name, adm2_name, adm3_name, ref_adm3_pcode = pcode)
  geo_ref4 <- geo_ref %>%
    filter(level == 4) %>%
    select(adm1_name, adm2_name, adm3_name, adm4_name, ref_adm4_pcode = pcode)

  janitor::get_dupes(geo_ref1, adm1_name)

  df_pcode <- df_match %>%
    distinct(pcode) %>%
    left_join(geo_ref, by = "pcode") %>%
    left_join(geo_ref1, by = c("adm0_name", "adm1_name")) %>%
    left_join(geo_ref2, by = c("adm1_name", "adm2_name")) %>%
    left_join(geo_ref3, by = c("adm1_name", "adm2_name", "adm3_name")) %>%
    left_join(geo_ref4, by = c("adm1_name", "adm2_name", "adm3_name", "adm4_name")) %>%
    select(adm0_name, pcode, ref_adm1_pcode:ref_adm4_pcode)

  df_ref_bind <- df_match %>%
    filter(!is.na(pcode)) %>%
    distinct(
      t_r_iso3 = adm0_name,
      adm1_t_target_area = adm1_name,
      adm2_t_target_area = adm2_name,
      adm3_t_target_area = adm3_name,
      adm4_t_target_area = adm4_name,
      ref_adm0_name,
      ref_adm1_name,
      ref_adm2_name,
      ref_adm3_name,
      ref_adm4_name,
      ref_pcode = pcode
    ) %>%
    left_join(df_pcode, by = c("t_r_iso3" = "adm0_name", "ref_pcode" = "pcode"))


  ## return --------------------------------------------------------------------
  dat_prep %>%
    left_join(df_ref_bind, by = adm_cols_orig)
}

pull_geo_data <- function(path, level, countries) {
  arrow::open_dataset(fs::path(path, level)) |> 
    dplyr::filter(adm0_iso3 %in% countries) |> 
    sf::st_as_sf()
}

make_geo_layer <- function(lvl, geo_path, countries) {
  lvl_upper <- toupper(lvl)
  lvl_lower <- tolower(lvl)
  epishiny::geo_layer(
    layer_name = lvl_upper,
    sf = pull_geo_data(geo_path, lvl_upper, countries),
    name_var = paste0(lvl_lower, "_name"),
    pop_var = paste0(lvl_lower, "_pop"),
    join_by = setNames(paste0(lvl_lower, "_pcode"), "pcode")
  )
}
