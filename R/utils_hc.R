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
pal10[7] <- "#e377c2"

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
hc_download_btns <- c("downloadPNG", "downloadJPEG", "downloadSVG")

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
