google_font <- function(font) {
  font <- stringr::str_replace_all(font, " ", "+")
  glue::glue("https://fonts.googleapis.com/css2?family={font}:wght@300;400;700&display=swap")
}

div_reactive <- function(...) {
  tags$div(class = "reactive-width", ...)
}

div_inline <- function(..., margin_right = TRUE) {
  display <- "display: inline-block;"
  margin <- ifelse(margin_right, "margin-right: 10px;", "")
  tags$div(style = paste(display, margin), ...)
}

col_auto <- function(...) {
  shiny::div(
    class = "col-auto",
    ...
  )
}

health_icon <- function(icon, type = "filled", format = "svg", height = 25) {
  type <- match.arg(type, c("filled", "outline", "negative"), several.ok = FALSE)
  format <- match.arg(format, c("svg", "png"), several.ok = FALSE)
  base_url <- "https://github.com/resolvetosavelives/healthicons/blob/main/public/icons"
  tags$img(
    src = glue::glue("{base_url}/{format}/{type}/{icon}.{format}?raw=true"),
    height = height
  )
}

flag_country <- function(iso3) {
  iso2 <- tolower(countrycode::countrycode(iso3, "iso3c", "iso2c"))
  cname <- countrycode::countrycode(iso3, "iso3c", "cow.name")
  url <- glue::glue("https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/{iso2}.svg")
  shiny::HTML(paste(tags$img(src = url, width = 20, height = 15), cname))
}

picker_opts <- function(actions = TRUE,
                        search = FALSE,
                        none_text = "All",
                        selected_text = "selected",
                        style = "btn-sm btn-outline-success",
                        ...) {
  shinyWidgets::pickerOptions(
    actionsBox = actions,
    liveSearch = search,
    selectedTextFormat = "count > 2",
    countSelectedText = paste("{0}", selected_text),
    noneSelectedText = none_text,
    dropupAuto = FALSE,
    style = style,
    ...
  )
}

add_user_to_navbar <- function(user) {
  shiny::insertUI(
    selector = ".navbar .container-fluid .navbar-collapse",
    ui = tags$ul(
      class = "nav navbar-nav navbar-right",
      tags$li(
        tags$a(tagList(shiny::icon("user"), user), href = NULL)
      )
    )
  )
}

add_logout_button <- function() {
  shiny::insertUI(
    selector = ".navbar .container-fluid .navbar-collapse",
    ui = tags$ul(
      class = "nav navbar-nav navbar-right",
      tags$li(
        div(
          style = "padding: 10px; padding-bottom: 0;",
          shiny::HTML(
            '<a href="https://reports.msf.net/secure/logout" class="btn btn-danger" role="button">Logout</a>'
          )
        )
      )
    )
  )
}

#' Custom box panel to display single values
value_panel <- function(title, n, width = 3) {
  div(
    class = glue::glue("col-sm-{width}"),
    div(
      class = "value-panel",
      h5(title, class = "vp-title"),
      n
    )
  )
}


tab_box_custom <- function(
    ...,
    id = NULL,
    selected = NULL,
    title = NULL,
    inputs = NULL,
    width = 6,
    height = NULL,
    side = c("left", "right")
) {
  side <- match.arg(side)
  content <- shiny::tabsetPanel(..., id = id, selected = selected)
  content$attribs$class <- "nav-tabs-custom"
  if (!is.null(height)) {
    content <- tagAppendAttributes(
      content,
      style = paste0("height: ", validateCssUnit(height))
    )
  }
  if (side == "right") {
    content$children[[1]] <- tagAppendAttributes(
      content$children[[1]],
      class = "pull-right"
    )
  }
  if (!is.null(title)) {
    if (side == "left") {
      titleClass <- "pull-right"
    } else {
      titleClass <- "pull-left"
    }
    content$children[[1]] <- htmltools::tagAppendChild(
      content$children[[1]],
      tags$li(class = paste("box-header", titleClass), h3(class = "box-title", style = "display: inline-block; margin-right: 15px;", title))
    )
  }
  if (!is.null(inputs)) {
    if (side == "left") {
      titleClass <- "pull-right"
    } else {
      titleClass <- "pull-left"
    }
    content$children[[1]] <- htmltools::tagAppendChild(
      content$children[[1]],
      tags$li(class = titleClass, purrr::map(inputs, div_inline))
    )
  }
  div(class = paste0("col-sm-", width), content)
}

box_custom <- function(
  ..., 
  title = NULL, 
  inputs = NULL, 
  input_right = NULL,
  footer = NULL, 
  status = NULL, 
  solidHeader = FALSE,
  background = NULL, 
  width = 6, 
  height = NULL, 
  collapsible = FALSE, 
  collapsed = FALSE,
  headerId = NULL
) {
  boxClass <- "box"
  if (solidHeader || !is.null(background)) {
    boxClass <- paste(boxClass, "box-solid")
  }
  if (!is.null(status)) {
    shinydashboard:::validateStatus(status)
    boxClass <- paste0(boxClass, " box-", status)
  }
  if (collapsible && collapsed) {
    boxClass <- paste(boxClass, "collapsed-box")
  }
  if (!is.null(background)) {
    shinydashboard:::validateColor(background)
    boxClass <- paste0(boxClass, " bg-", background)
  }
  style <- NULL
  if (!is.null(height)) {
    style <- paste0("height: ", shiny::validateCssUnit(height))
  }
  titleTag <- NULL
  if (!is.null(title)) {
    titleTag <- h3(class = "box-title", style = "display: inline-block; margin-right: 15px;", title)
  }
  collapseTag <- NULL
  if (collapsible) {
    buttonStatus <- status %OR% "default"
    collapseIcon <- if (collapsed) {
      "plus"
    } else {
      "minus"
    }
    collapseTag <- div(
      class = "box-tools pull-right",
      tags$button(class = paste0("btn btn-box-tool"), `data-widget` = "collapse", shiny::icon(collapseIcon))
    )
  }
  input_rightTag <- NULL
  if (!is.null(input_right)) {
    input_rightTag <- div(
      class = "box-tools pull-right",
      input_right
    )
  }
  headerTag <- NULL
  if (!is.null(titleTag) || !is.null(collapseTag)) {
    headerTag <- div(id = headerId, class = "box-header", titleTag, inputs, input_rightTag, collapseTag)
  }
  div(class = if (!is.null(width)) {
    paste0("col-sm-", width)
  }, div(class = boxClass, style = if (!is.null(style)) {
    style
  }, headerTag, div(class = "box-body", ...), if (!is.null(footer)) {
    div(class = "box-footer", footer)
  }))
}

box_w_inputs <- function(..., title, inputs = NULL, input_right = NULL, 
                         width = 6, height = NULL, footer = NULL, headerId = NULL) {
  box_custom(
    width = width, height = height, solidHeader = TRUE, footer = footer,
    title = title, inputs = purrr::map(inputs, div_inline), input_right = input_right, 
    headerId = headerId,
    ...
  )
}

box_w_dropdown_inputs <- function(..., title, inputs, width = 6, height = NULL, footer = NULL, btn_lab = "options", btn_id = NULL) {
  box_custom(
    width = width, height = height, solidHeader = TRUE, footer = footer,
    title = title, inputs = tags$div(
      id = btn_id, style = "display: inline-block;",
      shinyWidgets::dropdownButton(
        size = "sm", label = btn_lab, icon = shiny::icon("sliders"),
        inline = FALSE, width = 300, circle = FALSE, margin = 10,
        inputs
      )
    ),
    ...
  )
}

valueBoxSpark <- function(value, title, sparkobj = NULL, subtitle = NULL, info = NULL,
                          icon = NULL, color = "aqua", width = 3, href = NULL){

  shinydashboard:::validateColor(color)

  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")

  info_icon <- tags$small(
    tags$i(
      class = "fa fa-info-circle fa-lg",
      title = info,
      `data-toggle` = "tooltip",
      style = "color: rgba(255, 255, 255, 0.75);"
    ),
    # bs3 pull-right
    # bs4 float-right
    class = "pull-right float-right"
  )

  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      tags$small(title),
      if (!is.null(info)) info_icon,
      h3(value),
      if (!is.null(sparkobj)) sparkobj,
      if (!is.null(subtitle)) p(subtitle)
    ),
    # bs3 icon-large
    # bs4 icon
    if (!is.null(icon)) div(class = "icon-large icon", icon, style = "z-index; 0")
  )

  if (!is.null(href))
    boxContent <- a(href = href, boxContent)

  div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    boxContent
  )
}

