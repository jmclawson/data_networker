library(tidyverse)
library(shiny)
library(bslib)
library(r2d3)
library(network)
library(sna)
library(ggnetwork)
library(shinyjs)
library(colourpicker)
library(shadowtext)
source("R/helpers.R")
source("R/reactives.R")
source("ui_contents.R")

##### UI #####
ui <- function(request) { page_navbar(
  title = "data networker",
  id = "p",
  theme = bs_theme(
    "navbar-bg" = "#e8e8e8",
    "sidebar-bg" = "#FFFFFF",
    primary = "#6c75ad",
    version = 5,
    # bootswatch = "lux",
    ),
  header = tags$head(tags$style(
    HTML(
    "
    .navbar .navbar-brand {
      font-family: Courier;
      text-transform: lowercase;
    }
    .navbar .nav-item {
      text-transform: none;
    }
    .bslib-value-box {
      margin-bottom: 4px !important;
    }
    .bslib-sidebar-layout>.sidebar {
    background-color: white !important;
    }
    .navbar {
      padding-top: 10px !important;
      padding-bottom: 10px !important;
    }
    .selectize-input {
      max-height: 50px;
    }

    .export-links {
      width: 1.4em;
      display: inline-block;
    }

    label:empty {
      display: none;
    }

    .t-bottom {
      width: 100%;
      margin: 0 auto;
    }
    .t-entries {
      margin: 0.2em 20px;
      display: inline-block;
      vertical-align: middle;
      float: left;
    }
    .t-info {
      margin: -1.2em 20px 0 20px;
      display: inline-block;
      vertical-align: top;
      float: center;
    }
    .t-page {
      margin: -1.2em 0 0 50px;
      display: inline-block;
      vertical-align: top;
      float: right;
    }

      .bslib-sidebar-layout[data-collapsible-mobile='true']:not(.sidebar-right)>.collapse-toggle {
      width: 100px;
      }
          .bslib-sidebar-layout > .collapse-toggle::after {
      display: inline-block;
      font-size: 0.95rem;
      line-height: 1;
      margin-left: .4rem;
      vertical-align: middle;
          }

     @media (max-width: 575px) {
    .bslib-sidebar-layout > .collapse-toggle::after {
      content: 'Options';
    }
     }

      @media (min-width: 576px) {
        .bslib-sidebar-layout {
          position: relative;
          --sidebar-width: 250px;
        }

        .bslib-sidebar-layout .accordion {
          margin-top: -30px;
        }

        .bslib-sidebar-layout > .collapse-toggle {
          position: absolute;
          top: .5rem;
          z-index: 1030;
          width: 50px !important;
          background: var(--bs-body-bg);
          border: 1px solid var(--bs-border-color);
          border-radius: 999px;
          padding: .125rem .35rem;
          box-shadow: 0 2px 6px rgba(0,0,0,.08);
        }

        .bslib-sidebar-layout > .collapse-toggle[aria-expanded='true'] {
          left: calc(var(--sidebar-width) - 25px) !important;
  }

      }
          "
    )
  )),
  sidebar = sidebar_contents,
  nav_spacer(),
  tables_contents,
  ggplot2_contents,
  d3_contents,
  notes_contents,
  nav_spacer(),
  nav_item(
    tags$a(
      shiny::icon("github"), "Source",
      href = "https://github.com/jmclawson/data_networker/",
      target = "_blank"
    )
  )
)}

##### Bookmarking #####

enableBookmarking("url")

##### Server #####

server <- function(input, output, session) {
  values <- reactiveValues()

  ##### 1. independent params #####
  observeEvent(session$clientData$url_search, {
    query <- session$clientData$url_search |>
      parseQueryString()
    # saveRDS(session, "session.rds")
    # saveRDS(reactiveValuesToList(session$clientData), "client_session.rds")

    # set URL with `u`
    if (!is.null(query[['u']])) {
      updateTextAreaInput(
        session,
        inputId = "u",
        value = query[['u']])
    }
    # choose panel with `p`
    if (!is.null(query[['p']])) {
      nav_select(
        id = "p",
        selected = query[['p']],
        session)
    }
    # toggle sidebar with `s`
    if (!is.null(query[['s']])) {
      toggle_sidebar(
        id = "s",
        open = query[['s']] == 1, # open with s=1
        session)
    }
    # set accordion with `a`
    if (!is.null(query[['a']]) && query[['a']] %in% c("Import", "Adjust", "Choose", "Measure", "Customize")) {
      accordion_panel_close(
        id = "a",
        values = c(
          "Import",
          "Adjust",
          "Choose",
          "Measure",
          "Customize") |>
          {\(x) x[!x %in% query[['a']]]}()
      )
      accordion_panel_open(
        id = "a",
        value = query[['a']]
      )
    }

    # use color
    # custom color from one column
    if (!is.null(query[['color']])) {
      updateCheckboxInput(
        inputId = "color",
        value = query[['color']]
      )
    }
    if (!is.null(query[['color_branch']])) {
      updateRadioButtons(
        inputId = "color_branch",
        selected = query[['color_branch']])
    }
    # custom (manual) color
    if (!is.null(query[['color_cus']])) {
      colourpicker::updateColourInput(
        session,
        inputId = "color_cus",
        value = query[['color_cus']]
      )
    }
    # show arrow
    if (!is.null(query[['arrow']])) {
      updateCheckboxInput(
        inputId = "arrow",
        value = as.numeric(query[['arrow']]) == 1
      )
    }
    if (!is.null(query[['label']])) {
      updateCheckboxInput(
        inputId = "label",
        value = as.numeric(query[['label']]) == 1
      )
    }
    values$pend_ <- query
  }, ignoreInit = FALSE, once = TRUE)

  ##### 2. dependent params #####
  observe({
    req(the_middle())
    req(any(!is.null(values$pend_$separate_col), !is.null(values$pend_$split_col), !is.null(values$pend_$combine_col1)))

    query <- values$pend_

    if (!is.null(query[['source']])) {
    updateSelectInput(
      inputId = "source",
      choices = colnames(the_middle()),
      selected = query[['source']])
    }

    if (!is.null(query[['target']])) {
    updateSelectInput(
      inputId = "target",
      choices = colnames(the_middle()),
      selected = query[['target']])
    }

    if (!is.null(query[['separate_col']])) {
    updateSelectInput(
      inputId = "separate_col",
      choices = colnames(the_middle()),
      selected = query[['separate_col']])
    }

    if (!is.null(query[['split_col']])) {
    updateSelectInput(
      inputId = "split_col",
      choices = colnames(the_middle()),
      selected = query[['split_col']])
    }

    if (!is.null(query[['combine_col1']])) {
    updateSelectInput(
      inputId = "combine_col1",
      choices = colnames(the_middle()),
      selected = query[['combine_col1']])
    }

    if (!is.null(query[['combine_col2']])) {
    updateSelectInput(
      inputId = "combine_col2",
      choices = colnames(the_middle()),
      selected = query[['combine_col2']])
    }

    if (!is.null(query[['combine_name']])) {
      updateTextInput(
        inputId = "combine_name",
        value = query[['combine_name']])
    }
  })

  ##### 3. dependent sna params #####
  observe({
    req(the_result())
    req(values$pend_$do_sna)

    query <- values$pend_

    # measure network characteristics
    if (!is.null(query[['do_sna']])) {
      updateCheckboxInput(
        inputId = "do_sna",
        value = query[['do_sna']]
      )
    }
    if (!is.null(query[['directed']])) {
      updateRadioButtons(
        inputId = "directed",
        selected = query[['directed']]
      )
    }
    if (!is.null(query[['sna_add']])) {
      updateSelectInput(
        inputId = "sna_add",
        selected = query[names(query) == "sna_add"] |>
          unlist() |>
          unname()
      )
    }
  })

  ##### 4. dependent customization params #####
  observe({
    req(the_result())
    req(any(
      !is.null(values$pend_$weight_col),
      !is.null(values$pend_$label_col),
      !is.null(values$pend_$size_col),
      !is.null(values$pend_$legend),
      !is.null(values$pend_$color_col)))

    query <- values$pend_

    # weight edges
    if (!is.null(query[['weighted']])) {
      updateCheckboxInput(
        inputId = "weighted",
        value = query[['weighted']])
    }
    if (!is.null(query[['weight_col']])) {
      updateSelectInput(
        inputId = "weight_col",
        selected = query[['weight_col']],
        choices = c("", the_result_numeric() |>
                      colnames()))
    }
    # label charts
    if (!is.null(query[['label_col']])) {
      updateSelectInput(
        inputId = "label_col",
        selected = query[['label_col']],
        choices = c("", colnames(the_result()) |>
                      str_subset("target", negate = TRUE)))
    }
    # choose size column from existing
    if (!is.null(query[['size_col']])) {
      updateSelectInput(
        inputId = "size_col",
        selected = query[['size_col']],
        choices = c("", the_result_numeric() |>
                      colnames())
      )
    }
    # color from one column
    if (!is.null(query[['color_col']])) {
      updateSelectInput(
        inputId = "color_col",
        selected = query[['color_col']],
        choices = c("", colnames(the_result())))
    }
    # legend
    if (!is.null(query[['legend']])) {
      updateCheckboxInput(
        inputId = "legend",
        value = as.numeric(query[['legend']]) == 1
      )
    }
  })

  observe({
    if (!is.null(values)) {
      nullify_twins <- function(values, item, dependency = NULL) {
        # hold on item until dependency is null
        if (!is.null(dependency) && !is.null(values$pend_[[item]])) {
          return()
        }
        if (is.logical(input[[item]])) {
          if (!is.null(values$pend_[[item]]) && identical(as.integer(input[[item]]), as.integer(values$pend_[[item]]))) {
            values$pend_[[item]] <- NULL
            # print(paste("Nullifying", item))
          }
        } else if (!is.null(values$pend_[[item]]) && identical(input[[item]], values$pend_[[item]])) {
          values$pend_[[item]] <- NULL
          # print(paste("Nullifying", item))
        }
        values
      }

      values <- values |>
        #layout params
        nullify_twins("p") |>
        nullify_twins("a") |>
        nullify_twins("u") |>
        # wrangling params
        nullify_twins("source") |>
        nullify_twins("target") |>
        nullify_twins("separate_col") |>
        nullify_twins("split_col") |>
        nullify_twins("combine_col1") |>
        nullify_twins("combine_col2") |>
        nullify_twins("combine_name") |>
        # sna params
        nullify_twins("do_sna") |>
        nullify_twins("directed") |>
        nullify_twins("sna_add") |>
      # customizing params
        nullify_twins("arrow") |>
        nullify_twins("weighted") |>
        nullify_twins("weight_col") |>
        nullify_twins("label_col", "sna_add") |>
        nullify_twins("size_col", "sna_add") |>
        nullify_twins("label") |>
        nullify_twins("color") |>
        nullify_twins("color_branch") |>
        nullify_twins("color_col") |>
        nullify_twins("legend")
    } else {
      # print(paste("unfinished:", names(values$pend_)))
    }
  })

  ##### Notes tab #####
  output$link_pepys <- renderUI({
    base_url <- paste0(session$clientData$url_protocol, "//",
                       session$clientData$url_hostname,
                       if (session$clientData$url_port != "") paste0(":", session$clientData$url_port),
                       session$clientData$url_pathname)
    the_link <- paste0(base_url, "?a=Customize&p=ggplot2&u=https://raw.githubusercontent.com/jmclawson/data_networker/refs/heads/main/data/pepys_reciprocity-edges_extra.csv&arrow=1&color=1&label=1&do_sna=1&legend=0&sna_add=degree&directed=directed&size_col=degree&weighted=1&color_col=gender&color_branch=from%20column&label_col=source")
    HTML(paste0("<a href='", the_link, "'>Pepys reciprocity data</a>"))
  })

  output$link_star_wars <- renderUI({
    base_url <- paste0(session$clientData$url_protocol, "//",
                       session$clientData$url_hostname,
                       if (session$clientData$url_port != "") paste0(":", session$clientData$url_port),
                       session$clientData$url_pathname)
    the_link <- paste0(base_url, "?a=Customize&p=D3&s=0&u=https://raw.githubusercontent.com/evelinag/StarWars-social-network/refs/heads/master/networks/starwars-full-interactions.json&color=1&label=1&size_col=degree&weighted=1&color_col=group&weight_col=weight&color_branch=from%20column&label_col=source")
    HTML(paste0("<a href='", the_link, "'><em>Star Wars</em> characters</a>"))
  })

  output$link_les_miserables <- renderUI({
    base_url <- paste0(session$clientData$url_protocol, "//",
                       session$clientData$url_hostname,
                       if (session$clientData$url_port != "") paste0(":", session$clientData$url_port),
                       session$clientData$url_pathname)
    the_link <- paste0(base_url, "?a=Measure&p=ggplot2&u=https://raw.githubusercontent.com/mbostock/vega/066309624c45b1ab15e0abbc295f90878b2f33a7/docs/data/miserables.json&color=1&label=1&do_sna=1&legend=0&directed=undirected&size_col=degree&color_col=group&color_branch=from%20column&label_col=source&sna_add=degree")
    HTML(paste0("<a href='", the_link, "'><em>Les Misérables</em> characters</a>"))
  })

  ##### bookmarking #####
  output$share_url <- renderUI({
    # compare query params to default values
    ## (I only care about non-default values, and I
    ## only care about those named in defaults.)
    params_compare <- function(input, defaults) {
      params <- list()
      for (setting in names(defaults)) {
        if (!is.null(input[[setting]]) &&
            !identical(input[[setting]], defaults[[setting]])) {
          params[[setting]] <- input[[setting]] |>
            as.character() |>
            stringr::str_replace_all("^TRUE$", "1") |>
            stringr::str_replace_all("^FALSE$", "0")
        }
      }
      params
    }

    # handle inputs that accept multiple values
    construct_query_string <- function(params) {
      qs <- list()

      for (i in 1:length(params)) {
        this_name <- names(params)[i]
        this_value <- params[[i]]
        this_qs <- paste(this_name, this_value, sep = "=", collapse = "&")
        qs[[i]] <- this_qs
      }

      paste0(qs, collapse = "&")
    }

    # nice bookmarks
    build_bookmark_url <- function(input, defaults, session = getDefaultReactiveDomain()) {
      params <- params_compare(input, defaults)

      base_url <- paste0(session$clientData$url_protocol, "//",
                         session$clientData$url_hostname,
                         if (session$clientData$url_port != "") paste0(":", session$clientData$url_port),
                         session$clientData$url_pathname)

      if (length(params) > 0) {
        query_string <- construct_query_string(params)
        paste0(base_url, "?", query_string)
      } else {
        base_url
      }
    }
    defaults <- readRDS("defaults.rds")
    bookmark_url <- build_bookmark_url(input, defaults)


    div(
      tags$a(href = paste0(bookmark_url, "&s=0"),
             list(
               icon("link",
                    class = "export-links")),
             target = "_blank",
             title = "Link without sidebar"),
      tags$a(href = bookmark_url,
             list(
               "Shareable URL"),
             target = "_blank",
             title = "Right-click to copy link")
    )
  })

  ##### starting data #####
  starting_data <- reactive({
    if (!is.null(parseQueryString(session$clientData$url_search)[['u']])) {
      the_u <- parseQueryString(session$clientData$url_search)[['u']]

      if (stringr::str_detect(the_u, "^http")) {
        the_u |>
          get_online_file() |>
          load_network_file()
      } else {
        the_u |>
          load_network_file()
      }
    } else {
      readr::read_csv("data/pepys_reciprocity-edges_extra.csv", show_col_types = FALSE) |>
        select(-date)
    }
  })

  ##### from_input() #####

  from_input <- reactive({
    if (input$data_source == "URL") {
      req(input$u) |>
        get_online_file() |>
        load_network_file()
    } else {
      file <- req(input$file)
      file$datapath |>
        load_network_file()
    }
  }) |>
    bindEvent(
      input$load_button)

  ##### the_df() #####

  the_df <- reactive({
    if (input$load_button == 0) {
      starting_data()
    } else {
      from_input()
    }
  })

  ##### observe changes to the_middle() #####
  observe({
    updateSelectInput(
      inputId = "source",
      choices = colnames(the_middle()),
      selected =
        if (input$source %in% colnames(the_middle())) {
          input$source
        } else if ("source" %in% colnames(the_middle())) {
          "source"
        } else if ("Source" %in% colnames(the_middle())) {
          "Source"
        } else {
          ""
        })

    updateSelectInput(
      inputId = "target",
      choices = colnames(the_middle()),
      selected =
        if (input$target %in% colnames(the_middle())) {
          input$target
        } else if ("target" %in% colnames(the_middle())) {
          "target"
        } else if ("Target" %in% colnames(the_middle())) {
          "Target"
        } else {
          ""
        })
    req(is.null(values$pend_))

    updateSelectInput(
      inputId = "separate_col",
      choices = colnames(the_middle()),
      selected =
        if (input$separate_col %in% colnames(the_df())) {
          input$separate_col
        } else {
          ""
        })

    updateSelectInput(
      inputId = "split_col",
      choices = colnames(the_middle()),
      selected =
        if (input$split_col %in% colnames(the_df())) {
          input$split_col
        } else {
          ""
        })

    updateSelectInput(
      inputId = "combine_col1",
      choices = colnames(the_middle()),
      selected =
        if (input$combine_col1 %in% colnames(the_df())) {
          input$combine_col1
        } else {
          ""
        })

    updateSelectInput(
      inputId = "combine_col2",
      choices = colnames(the_middle()),
      selected =
        if (input$combine_col2 %in% colnames(the_df())) {
          input$combine_col2
        } else {
          ""
        })
  })

  observeEvent(input$do_split,{
    updateTextInput(
      inputId = "split",
      value =
        if (input$do_split == FALSE) {
          ""
        })},
    ignoreInit = TRUE)

  observeEvent(input$do_separate,{
    updateTextInput(
      inputId = "separate",
      value =
        if (input$do_separate == FALSE) {
          ""
        })},
    ignoreInit = TRUE)

  observeEvent(input$label,{
    updateSelectInput(
      inputId = "label_col",
      selected =
        if (input$label == FALSE) {
          ""
        } else if ("source" %in% colnames(the_result())) {
          "source"
        })},
    ignoreInit = TRUE)

  observeEvent(input$weighted,{
    updateSelectInput(
      inputId = "weight_col",
      selected =
        if (input$weighted == FALSE) {
          ""
        } else if ("weight" %in% colnames(the_result())) {
          "weight"
        })},
    ignoreInit = TRUE)

  ##### Make changes on load file #####
  observeEvent(c(input$load_button, input$file),{
    updateSelectInput(
      inputId = "separate_col",
      choices = colnames(the_middle()))

    updateSelectInput(
      inputId = "split_col",
      choices = colnames(the_middle()))

    updateTextInput(
      inputId = "separate",
      value = "")

    updateTextInput(
      inputId = "split",
      value = "")

    updateSelectInput(
      inputId = "combine_col1",
      choices = colnames(the_middle()),
      selected = "")

    updateSelectInput(
      inputId = "combine_col2",
      choices = colnames(the_middle()),
      selected = "")

    updateSelectInput(
      inputId = "source",
      selected =
        if ("source" %in% colnames(the_middle())) {
          "source"
        } else {
          ""
        })

    updateSelectInput(
      inputId = "target",
      selected =
        if ("target" %in% colnames(the_middle())) {
          "target"
        } else {
          ""
        })

    updateSelectInput(
      inputId = "separate_col",
      selected = "")

    updateSelectInput(
      inputId = "split_col",
      selected = "")

    # updateSelectInput(
    #   inputId = "size_col",
    #   selected = "",
    #   choices = c("", colnames(the_result())))

    updateCheckboxInput(inputId = "do_separate", value = FALSE)

    updateCheckboxInput(inputId = "do_split", value = FALSE)

    updateCheckboxInput(inputId = "do_combo", value = FALSE)

    # updateCheckboxInput(inputId = "do_sna", value = FALSE)

    updateCheckboxInput(inputId = "weighted", value = FALSE)

  })

  ##### observe changes to the_result() #####
  observe({
    req(the_result())
    updateSelectInput(
        inputId = "size_col",
        selected = if (isolate(input$size_col) %in% colnames(the_result())) isolate(input$size_col) else "",
        choices = c("", colnames(the_result_numeric())))
    updateSelectInput(
      inputId = "color_col",
      selected = if (isolate(input$color_col) %in% colnames(the_result())) isolate(input$color_col) else "",
      choices = c("", colnames(the_result())))
    updateSelectInput(
      inputId = "weight_col",
      selected = if (isolate(input$weight_col) %in% colnames(the_result())) isolate(input$weight_col) else "",
      choices = c("", colnames(the_result_numeric())))
    updateSelectInput(
      inputId = "label_col",
      choices = c("", colnames(the_result())))
  })

  the_middle <- reactive({
    my_middle <- the_df()

    if (input$do_combo) {
      my_middle <- my_middle |>
        combine_columns(!!sym(input$combine_col1),
                        !!sym(input$combine_col2),
                        input$combine,
                        !!sym(input$combine_name))
    }

    if (nchar(input$separate) > 0 &&
        input$separate_col != "") {
      my_middle <- my_middle |>
        extend_column(!!sym(input$separate_col), input$separate)
    }

    if (nchar(input$split) > 0 &&
        input$split_col != "") {
      my_middle <- my_middle |>
        divide_column(!!sym(input$split_col),
                      input$split,
                      !!sym(input$split_col1),
                      !!sym(input$split_col2))
    }

    my_middle
  })

  the_result <- reactive({
    the_middle() |>
      get_result_df(input)
  })

  the_result_numeric <- reactive({
    the_result() |>
      select(where(is.numeric))
  })

  the_network <- reactive({
    validate(
      need(input$source %in% colnames(the_middle()), "Please choose a valid `source` column."),
      need(input$target %in% colnames(the_middle()), "Please choose a valid `target` column.")
    )
    the_result() |>
      get_network_df(input)
  })

  the_width <- reactive({
    input$width
  })

  the_height <- reactive({
    input$height
  })

  output$num_nodes <- renderText({
    validate(
      need(input$source %in% colnames(the_middle()), "?"),
      need(input$target %in% colnames(the_middle()), "?")
    )
    c(the_result()$source, the_result()$target) |>
      unique() |>
      {\(x) x[!is.na(x)]}() |>
      length()
  })

  output$num_edges <- renderText({
    validate(
      need(input$source %in% colnames(the_middle()), "?"),
      need(input$target %in% colnames(the_middle()), "?")
    )
    the_result() |>
      drop_na(source, target) |>
      distinct() |>
      nrow()
  })

  output$num_components <- renderText({
    validate(
      need(input$source %in% colnames(the_middle()), "?"),
      need(input$target %in% colnames(the_middle()), "?")
    )
    the_result() |>
      drop_na(source, target) |>
      network::network(multiple = TRUE) |>
      sna::components("weak")
  })

  output$d3 <- renderD3({
    validate(
      need(input$source %in% colnames(the_middle()), "Please choose a valid `source` column."),
      need(input$target %in% colnames(the_middle()), "Please choose a valid `target` column.")
    )
    the_result() |>
      df2d3_json(
        color_col = ifelse(
          input$color_branch == "from column",
          input$color_col,
          ""),
        label_col = if (input$label) {input$label_col} else {"source"},
        degree_col = input$size_col,
        weight_col = input$weight_col) |>
      r2d3::r2d3(
        d3_version = 4,
        script = "forcegraph.js",
        options = list(
          show_labels = input$label,
          default_color = input$color_cus))
  })

  output$df_contents <- renderTable({
    the_df()
  })

  output$contents <- DT::renderDataTable({
    DT::datatable(
      the_result(),
      # filter = "top",
      # editable = TRUE,
      options = list(
        dom = '<frt><"t-bottom"<"t-entries"l><"t-info"i><"t-page"p>>',
        server = FALSE
        # searching = TRUE
      ))
  })

  output$ggt <- renderTable({
    the_network()
  })

  output$ggv <- renderPlot({
    the_data <- the_network() |>
      prepare_plot_df(input)
    # write_csv(the_data, "the_data.csv")
    # saveRDS(reactiveValuesToList(input), "input.rds")

    make_plot(the_data, input)
  })

  output$download_csv <- downloadHandler(
    filename = filename_handler("csv"),
    content = function(file) {
      the_result() |>
        readr::write_csv(file)
    }
  )

  output$download_json <- downloadHandler(
    filename = filename_handler("json"),
    content = function(file) {
      the_result() |>
        df2d3_json() |>
        jsonlite::write_json(file, pretty = TRUE)
    }
  )

  output$download_png <- downloadHandler(
    filename = filename_handler("png"),
    content = function(file) {
      ggsave(file, width = 7, height = 7)
    }
  )
  output$download_pdf <- downloadHandler(
    filename = filename_handler("pdf"),
    content = function(file) {
      ggsave(file, width = 10, height = 10)
    }

  )

  filename_handler <- function(filetype = "pdf") {
    function() {
      if (input$data_source == "URL" && input$u != "") {
        input$u |>
          stringr::str_extract("[a-z A-Z 0-9 \\- _]+[.]{1}+[a-zA-Z]{1,4}$") |>
          str_remove_all("[.].*$") |>
          paste0(".", filetype)
      } else if (input$data_source == "upload" && !is.null(input$file)) {
        input$file |>
          str_remove_all("[.].*$") |>
          paste0(".", filetype)
      } else {
        "network" |>
          paste0(".", filetype)
      }
    }
  }

  observeEvent(input$p, {
    if (input$p == "D3") {
      hide("layout")
      hide("arrow")
      hide("curve")
      hide("legend")
      hide("weight_from")
      updateSelectInput(
        inputId = "weight_col",
        label = "from column")
    } else {
      show("layout")
      show("arrow")
      show("curve")
      show("legend")
      show("weight_from")
      updateSelectInput(
        inputId = "weight_col",
        label = "")
    }
  })


}

# Run the application
shinyApp(ui = ui, server = server)
