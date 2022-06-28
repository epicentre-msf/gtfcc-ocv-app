# langauge
# hc_lang <- getOption("highcharter.lang")
# hc_lang$shortMonths <- format(ISOdate(2000, 1:12, 1), "%b")
# hc_lang$months <- format(ISOdate(2000, 1:12, 1), "%B")
# hc_lang$weekdays <- c("dimanche", "lundi", "mardi", "mecredi", "jeudi", "vendredi", "samedi")
# options(highcharter.lang = hc_lang)

pal20 <- c(
  "#4E79A7FF", "#A0CBE8FF", "#F28E2BFF", "#FFBE7DFF", "#59A14FFF",
  "#8CD17DFF", "#B6992DFF", "#F1CE63FF", "#499894FF", "#86BCB6FF",
  "#E15759FF", "#FF9D9AFF", "#79706EFF", "#BAB0ACFF", "#D37295FF",
  "#FABFD2FF", "#B07AA1FF", "#D4A6C8FF", "#9D7660FF", "#D7B5A6FF"
)

pal10 <- leaflet.minicharts::d3.schemeCategory10

# pal10 <- c(
#   "#4E79A7FF", "#F28E2BFF", "#E15759FF", "#76B7B2FF", "#59A14FFF",
#   "#EDC948FF", "#B07AA1FF", "#FF9DA7FF", "#9C755FFF", "#BAB0ACFF"
# )

dark2 <- c(
  "steelblue", "#1B9E77", "#D95F02", "#7570B3", "#E7298A",
  "#66A61E", "#E6AB02", "#A6761D", "#666666"
)


fntfmly <- '"Roboto Condensed",-apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,"Helvetica Neue",Arial,sans-serif,"Apple Color Emoji","Segoe UI Emoji","Segoe UI Symbol";'

hc_opts <- getOption("highcharter.chart")
hc_opts$colors <- pal10
hc_opts$plotOptions$column <- list(zIndex = 2, stacking = "normal", groupPadding = 0.05, pointPadding = 0.05, borderWidth = 1, borderColor = "white")
hc_opts$plotOptions$bar <- list(zIndex = 2, stacking = "normal", groupPadding = 0.05, pointPadding = 0.05, borderWidth = 0.05)
hc_opts$credits <- list(enabled = FALSE, href = "", style = list(fontFamily = fntfmly, fontSize = "10px", fontStyle = "italic", cursor = "default"))
hc_opts$exporting <- list(enabled = FALSE)
hc_opts$title <- list(text = NULL)

options(
  highcharter.chart = hc_opts,
  highcharter.theme =
    highcharter::hc_theme_smpl(
      chart = list(style = list(fontFamily = fntfmly)),
      title = list(style = list(fontFamily = fntfmly)),
      subtitle = list(style = list(fontFamily = fntfmly)),
      plotOptions = list(line = list(marker = list(enabled = FALSE)))
    )
)

#hc_download_btns <- c("downloadPNG", "downloadJPEG", "downloadSVG", "separator", "downloadCSV", "downloadXLS")
hc_download_btns <- c("viewFullscreen", "separator", "downloadPNG", "downloadJPEG", "downloadSVG")

my_hc_export <- function(
  hc,
  title,
  subtitle,
  credits = "GTFCC | Epicentre",
  colors = dark2,
  width = 600,
  height = 350,
  dl_buttons = hc_download_btns,
  dl_text = "Download",
  filename = "GTFCC-"
) {

  set_hc_val <- function(first, second) {
    if (!missing(first)) {
      out <- first
    } else if (!is.null(second)) {
      out <- second
    } else {
      out <- NULL
    }
    return(out)
  }

  title <- set_hc_val(title, hc$x$hc_opts$title$text)
  subtitle <- set_hc_val(subtitle, hc$x$hc_opts$subtitle$text)
  colors <- set_hc_val(colors, hc$x$hc_opts$colors)
  credits <- set_hc_val(credits, hc$x$hc_opts$credits$text)

  legend_title <- stringr::str_remove(hc$x$hc_opts$legend$title$text, "\\(Click to hide\\)")

  highcharter::hc_exporting(
    hc,
    enabled = TRUE,
    sourceWidth = width,
    sourceHeight = height,
    buttons = list(contextButton = list(menuItems = dl_buttons, text = dl_text)),
    filename = paste0(filename, Sys.Date()),
    csv = list(dateFormat = "%d/%m/%Y"),
    tableCaption = "",
    useMultiLevelHeaders = FALSE,
    formAttributes = list(target = "_blank"),
    chartOptions = list(
      title = list(text = title),
      subtitle = list(text = subtitle),
      credits = list(enabled = TRUE, text = credits),
      colors = colors,
      legend = list(title = list(text = legend_title)),
      xAxis = list(plotBands = list()), # remove plotbands
      rangeSelector = list(enabled = FALSE),
      navigator = list(enabled = FALSE),
      # plotOptions = list(series = list(dataLabels = list(enabled = TRUE, format="{point.y:,.0f}"))),
      chart = list(
        backgroundColor = "#fff"
        # events = list(
        #   load = JS(paste0("function () {
        #     this.renderer.image(
        #     'https://epicentre.msf.org/sites/default/files/logo_Epicentre_1.png',", width - 176, ", -5, 176, 45)
        #     .add();
        #   }
        #   "))
        # )
      )
    )
  )
}


hc_theme_sparkline_vb <- function(...) {

  theme <- list(
    chart = list(
      backgroundColor = NULL,
      margins = c(0, 0, 0, 0),
      spacingTop = 0,
      spacingRight = 0,
      spacingBottom = 0,
      spacingLeft = 0,
      plotBorderWidth = 0,
      borderWidth = 0,
      style = list(overflow = "visible")
    ),
    xAxis = list(
      visible = FALSE,
      endOnTick = FALSE,
      startOnTick = FALSE
    ),
    yAxis = list(
      visible = FALSE,
      endOnTick = FALSE,
      startOnTick = FALSE
    ),
    tooltip = list(
      outside = FALSE,
      shadow = FALSE,
      borderColor = "transparent",
      botderWidth = 0,
      backgroundColor = "transparent",
      style = list(textOutline = "5px white")
    ),
    plotOptions = list(
      series = list(
        marker = list(enabled = FALSE),
        lineWidth = 2,
        shadow = FALSE,
        fillOpacity = 0.25,
        color = "#FFFFFFBF",
        fillColor = list(
          linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
          stops = list(
            list(0.00, "#FFFFFF00"),
            list(0.50, "#FFFFFF7F"),
            list(1.00, "#FFFFFFFF")
          )
        )
      )
    ),
    credits = list(
      enabled = FALSE,
      text = ""
    )
  )

  theme <- structure(theme, class = "hc_theme")

  if (length(list(...)) > 0) {
    theme <- hc_theme_merge(
      theme,
      hc_theme(...)
    )
  }

  theme
}

hc_as_pyramid <- function(
  df_data,
  age_var,
  sex_var,
  male_level = "Masculin",
  female_level = "Féminin",
  age_breaks = c(0, 5, 18, 25, 35, 50, Inf),
  age_labels = c("<5", "5-17", "18-24", "25-34", "35-49", "50+"),
  value_name = "Cas validés",
  value_digit = 0,
  value_unit = "",
  title = NULL,
  xlab = "Cas validés",
  ylab = "Tranche d'âge"
) {

  missing_sex <- sum(is.na(df_data[[age_var]]))
  missing_age <- sum(is.na(df_data[[sex_var]]))

  # as_sum <- df_data %>%
  #   dplyr::filter(!is.na(.data[[sex_var]])) %>%
  #   dplyr::group_by(.data[[sex_var]]) %>%
  #   dplyr::summarise(
  #     n = n(),
  #     median = median(.data[[age_var]], na.rm = TRUE),
  #     mean = mean(.data[[age_var]], na.rm = TRUE),
  #     .groups = "drop"
  #   ) %>%
  #   dplyr::mutate(dplyr::across(where(is.numeric), round, 1)) %>%
  #   dplyr::mutate(pcnt = n / sum(n))
  #
  # as_sum_m <- dplyr::filter(as_sum, .data[[sex_var]] == male_level)
  # as_sum_f <- dplyr::filter(as_sum, .data[[sex_var]] == female_level)
  #
  # colours <- c("#f15f36", "#19a0aa")
  #
  # info_text <- glue::glue(
  #   "<p style=\"font-weight: bold; color:{colours[2]};\">Males</p>:   {scales::number(as_sum_m$n)} ({scales::percent(as_sum_m$pcnt)}), Median age: {as_sum_m$median}, Mean age: {as_sum_m$mean}<br />
  #    <p style=\"font-weight: bold; color:{colours[1]};\">Females</p>: {scales::number(as_sum_f$n)} ({scales::percent(as_sum_f$pcnt)}), Median age: {as_sum_f$median}, Mean age: {as_sum_f$mean}"
  # )

  df_age_sex <- df_data %>%
    dplyr::mutate(age_group = cut(.data[[age_var]], breaks = age_breaks, labels = age_labels, include.lowest = TRUE, right = FALSE)) %>%
    dplyr::count(.data[[sex_var]], age_group) %>%
    dplyr::mutate(n = dplyr::if_else(.data[[sex_var]] == male_level, -n, n)) %>%
    tidyr::complete(.data[[sex_var]], age_group, fill = list(n = 0)) %>%
    dplyr::filter(!is.na(.data[[sex_var]]), !is.na(age_group)) %>%
    dplyr::arrange(.data[[sex_var]], age_group)

  max_value <- max(abs(df_age_sex$n))
  x_levels <- levels(df_age_sex$age_group)
  x_levels <- x_levels[x_levels != "(Unknown)"]
  xaxis <- list(categories = x_levels, reversed = FALSE, title = list(text = ylab))

  series <- df_age_sex %>%
    dplyr::group_by(.data[[sex_var]]) %>%
    dplyr::arrange(age_group) %>%
    dplyr::do(data = .$n) %>%
    dplyr::ungroup() %>%
    dplyr::rename(name = .data[[sex_var]]) %>%
    highcharter::list_parse()

  colours <- c("#f15f36", "#19a0aa")

  hc_out <- highcharter::highchart() %>%
    highcharter::hc_chart(type = "bar") %>%
    highcharter::hc_add_series_list(series) %>%
    highcharter::hc_plotOptions(
      series = list(stacking = "normal"),
      bar = list(groupPadding = 0.05, pointPadding = 0.05, borderWidth = 0.05)
    ) %>%
    highcharter::hc_yAxis(
      title = list(text = xlab),
      labels = list(
        formatter = highcharter::JS("function(){ return Math.abs(this.value); }")
      ),
      min = -max_value,
      max = max_value,
      allowDecimals = FALSE
    ) %>%
    highcharter::hc_xAxis(
      xaxis,
      rlist::list.merge(xaxis, list(opposite = TRUE, linkedTo = 0))
    ) %>%
    highcharter::hc_colors(colours) %>%
    highcharter::hc_tooltip(
      shared = FALSE,
      formatter = highcharter::JS(sprintf("function () { return '<b>' + this.series.name + ', age ' + this.point.category + 'y</b><br/>' + '%s: ' + Highcharts.numberFormat(Math.abs(this.point.y), %s)+'%s';}", value_name, value_digit, value_unit))
    ) %>%
    highcharter::hc_legend(enabled = TRUE, reversed = TRUE, verticalAlign = "top", align = "center") %>%
    highcharter::hc_title(text = NULL)
  #highcharter::hc_subtitle(text = info_text, align = "left")

  if (sum(missing_age, missing_sex) > 0) {
    hc_out <- hc_out %>%
      highcharter::hc_credits(enabled = TRUE, text = glue::glue("Missing data: Age ({scales::number(missing_age)}), Sex ({scales::number(missing_sex)})"))
  }

  hc_out
}





