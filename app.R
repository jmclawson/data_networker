library(tidyverse)
library(shiny)
library(bslib)
library(r2d3)
library(network)
library(sna)
library(ggnetwork)
library(shinyjs)
library(colourpicker)
source("R/helpers.R")
source("R/reactives.R")

##### UI #####
ui <- page_navbar(
  title = "data networker",
  id = "whole_page",
  theme = bs_theme(
    "navbar-bg" = "#e8e8e8",
    "sidebar-bg" = "#FFFFFF",
    primary = "#6c75ad",
    version = 5,
    bootswatch = "lux",
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

    .bslib-sidebar-layout > .collapse-toggle::after {
      content: 'Options';
    }
          "
    )
  )),
  sidebar = sidebar(
    open = list(desktop = "always", mobile = "closed"),
  accordion(
    title = "what?",
    id = "main_sidebar",
    multiple = TRUE,
    accordion_panel(
      "Load Data",
      icon = bsicons::bs_icon("cloud-arrow-up"),
      shiny::radioButtons(
        "load_data","Source:",
        choiceValues = c(
          "Samuel Pepys",
          "Star Wars",
          "Les Miserables",
          "import CSV"),
        choiceNames = list(
          "Samuel Pepys",
          em("Star Wars"),
          em("Les Misérables"),
          "import CSV:")),
      conditionalPanel(
        condition = "input.load_data == 'import CSV'",
        fileInput("file1", "", accept = ".csv"))
        ),
    accordion_panel(
      "Adjust",
      icon = bsicons::bs_icon("dpad"),
      checkboxInput("do_separate",
                    label = span(bsicons::bs_icon("arrows-expand"), "Separate one column's values into multiple rows."),
                    value = FALSE
      ),
      conditionalPanel(
        condition = "input.do_separate",
        wellPanel(
          selectInput("separate_col",
                      "Column",
                      choices = c("")),
          textInput("separate", "Separate at", placeholder = "(e.g., comma, underscore, space)")),
        p()
      ),
      checkboxInput("do_split",
                    label = span(bsicons::bs_icon("arrows-expand-vertical"), "Split one column into two."),
                    value = FALSE
      ),
      conditionalPanel(
        condition = "input.do_split",
        wellPanel(
          selectInput("split_col",
                      "Column",
                      choices = c("")),
          textInput("split", "Split at", placeholder = "(e.g., comma, underscore, space)"),
          textInput("split_col1", "New column name 1", value = "col1"),
          textInput("split_col2", "New column name 2", value = "col2")
        ),
        p()
      ),
      checkboxInput("do_combo",
                    label = span(bsicons::bs_icon("arrows-collapse-vertical"), "Combine two columns into one."),
                    value = FALSE
      ),
      conditionalPanel(
        condition = "input.do_combo",
        wellPanel(
          selectInput("combine_col1",
                      "Column 1",
                      choices = c("")),
          selectInput("combine_col2",
                      "Column 2",
                      choices = c("")),
          textInput("combine", "Combine at",
                    placeholder = "(e.g., comma, underscore, space)"),
          textInput("combine_name", "Name", value = "combo")
        )
        )
      ),
    accordion_panel(
      "Choose", id = "enforce_columns",
      icon = bsicons::bs_icon("list-check"),
      selectInput("source", "Source column", choices = c("source")),
      selectInput("target", "Target column", choices = c("target"))),
    accordion_panel(
      "Measure",
      icon = bsicons::bs_icon("node-plus"),
      checkboxInput("do_sna",
                    label = "Add network measurements.",
                    value = FALSE
      ),
      conditionalPanel(
        condition = "input.do_sna",
        wellPanel(
          radioButtons("choose_directedness",
                       "Graph mode",
                       choices = c("directed",
                                   "undirected")),
          selectInput("add_measures",
                      label = "Choose measurements",
                      multiple = TRUE,
                      choices = c("degree", "betweenness", "closeness", "prestige", "stress_centrality")
          ),
          p("Degree reports total (in- and out-degree) values. All measurements use the",
            a(href = "https://cran.r-project.org/web/packages/sna/index.html", "sna"), "package.")
        )
      )
    ),
    accordion_panel(
      "Export",
      icon = bsicons::bs_icon("cloud-download"),
      downloadLink("download_csv", "Gephi edges file"),
      downloadLink("download_json", "JSON file for D3"),
      downloadLink("download_png", "Plot as PNG"),
      downloadLink("download_pdf", "Plot as PDF")),
    accordion_panel(
      "Customize Plot",
      icon = bsicons::bs_icon("palette"),
      selectInput("layout_choice",
                  "Network layout",
                  choices = c(
                    "circle", "eigen", "fruchtermanreingold",
                    "hall", "kamadakawai", "mds",
                    "princoord", "random", "spring",
                    "target"),
                  selected = "fruchtermanreingold"),
      checkboxInput("show_arrow",
                    label = "Directed",
                    value = FALSE
      ),
      checkboxInput("show_weight",
                    label = "Weighted",
                    value = FALSE
      ),
      conditionalPanel(
        condition = "input.show_weight",
        wellPanel(
          radioButtons("weight_from",
                       "from",
                       choices = c("count", "column"),
                       selected = "count"),
          conditionalPanel(
            condition = "input.weight_from == 'column'",
            selectInput("weight_col",
                        "column:",
                        choices = c(""))
          )
        ),
        p()
      ),
      checkboxInput("show_color",
                    label = "Color",
                    value = FALSE
      ),
      conditionalPanel(
        condition = "input.show_color",
        wellPanel(
          radioButtons(
            inputId = "color_branch",
            label = "Choose color source",
            selected = "custom",
            choices = c("custom", "from column")),
          conditionalPanel(
            condition = "input.color_branch == 'custom'",
            colourpicker::colourInput(
              "custom_col",
              label = NULL,
              value = "#6c75adaa",
              allowTransparent = TRUE,
              closeOnClick = TRUE)
          ),
          conditionalPanel(
            condition = "input.color_branch == 'from column'",
            selectInput(
              "color_col",
              "Color points by column",
              choices = c(""))
          )
        ),
        p()
      ),
      checkboxInput("show_label",
                    label = "Label",
                    value = FALSE
      ),
      conditionalPanel(
        condition = "input.show_label",
        wellPanel(
          selectInput("label_col",
                      "Label by values in column",
                      choices = c(""))
        ),
        p()
      ),
      selectInput("size_col",
                  "Point size by column",
                  choices = c("")),
      numericInput("show_curve", "Curviness",
                   value = 0.1, step = 0.1),
      checkboxInput("show_legend",
                    label = "Show legend",
                    value = TRUE
      ),
      card(p(a(href = "https://ggplot2.tidyverse.org", "ggplot2"), "visualization uses", a(href = "https://briatte.github.io/ggnetwork/", "ggnetwork"), "to calculate geometries for nodes and edges. Network layouts listed here are from", a(href = "https://cran.r-project.org/web/packages/sna/index.html", "sna"), ".")
    ))
    )),
    nav_spacer(),
    nav_panel(
      title = "Tables",
      icon = icon("table"),
      shinyjs::useShinyjs(),
      card(id = "original",
           card_header("original CSV data"),
           max_height = 300,
           full_screen = TRUE,
           tableOutput("csv_contents")),
      fixedRow(
        layout_columns(
          col_widths = c(9,3),
          card(
            max_height = 380,
            full_screen = TRUE,
            tabsetPanel(
              tabPanel(
                title = "Adjusted data",
                tableOutput("contents")),
              tabPanel(
                title = "Geometry for ggplot2",
                tableOutput("ggt"))
            )
          ),
          column(
            12,
            fluidRow(
              value_box(
                title = "Nodes",
                max_height = (380/3)-2,
                fill = TRUE,
                value = textOutput("num_nodes"),
                showcase = bsicons::bs_icon("diagram-3", size = "0.8em"),
                showcase_layout = "top right",
                theme = "primary text-white"
              )
            ),
            fluidRow(
              value_box(
                title = "Edges",
                max_height = (380/3)-3,
                fill = TRUE,
                value = textOutput("num_edges"),
                showcase = bsicons::bs_icon("bezier2", size = "0.8em"),
                showcase_layout = "top right",
                theme = "secondary"
              )
            ),
            fluidRow(
              value_box(
                title = span("Connected", br(), "Components"),
                max_height = (380/3)-3,
                fill = TRUE,
                value = textOutput("num_components"),
                showcase = bsicons::bs_icon("layout-wtf", size = "0.6em"),
                showcase_layout = "top right",
                theme = "text-blue"
              )
            )
          )
        )
      )
      ),
    nav_panel(
      title = "ggplot2",
      value = "ggplot2",
      icon = icon("circle-nodes"),
      plotOutput("ggv",
                        height = "90vh")),
    nav_panel(
      title = "D3",
      value = "D3",
      icon = icon("square-js"),
      d3Output("d3",
               height = "90vh"),
      full_screen = TRUE),
    nav_panel(
      title = "Notes",
      icon = icon("circle-info"),
      layout_columns(
        uiOutput("num_connectedness")
      ),
      layout_columns(
        card(
          card_header("Methods"),
          p("Pepys data from", em(a(href = "https://www.pepysdiary.com", "The Diary of Samuel Pepys")), "showing reported reciprocity of social favors and gifts in the first week of April 1667. It was collected from the diary by Paula Chan, James Clawson, Caroline Greer, Joseph Stuart, and Sarah Tew as part of a", a(href="https://mathhumanists.org", "Mathematical Humanists"), "workshop led by Jessica Otis and Ashley Sanders."),
          p(em("Star Wars"), "data is from Evelina Gabasova, with methodology laid out in a", a(href= "https://evelinag.com/blog/2015/12-15-star-wars-social-network/index.html#how", "blog post"), "and data recorded in a", a(href = "https://github.com/evelinag/StarWars-social-network/blob/master/networks/starwars-full-interactions.json", "JSON file on GitHub.")),
          p(em("Les Miserables"), "data is originally from Donald Knuth, prepared in this version from the", a(href = "https://raw.githubusercontent.com/mbostock/vega/066309624c45b1ab15e0abbc295f90878b2f33a7/docs/data/miserables.json", "JSON file"), "made available by", a(href = "https://bost.ocks.org/mike/miserables/", "Mike Bostock.")),
          # p("The network calculations here don't normalize values between 0 and 1. Additionally, I haven't compared results to Gephi, so you should probably be consistent in the tools you use for calculating these values. (That said,", a(href = "https://cran.r-project.org/web/packages/sna/index.html", "sna"), "is a trusted library with more than 2 million downloads from CRAN, so it's probably dependable even if it calculates things differently than Gephi.)"),
          # p("D3 nodes might seem to go missing when data changes. They're still there! Find them tucked away trying to hide in the upper-left corner of the page. They'll shuffle back into place when you click them. Alternatively, try toggling the sidebar.")
          ),
      card(
        card_header("shinyapps.io"),
        p("This page is hosted on a free account with limitations on time and processing power, so don't be alarmed if it gets slow. I've also", a(href = "https://github.com/jmclawson/data_networker", "shared the source code"), "if you'd like to run it on your own machine, which is much faster than running on a server over the Internet."),
        p("A note on privacy: I can't see what you're uploading, but I do have access to logs that show when there's a problem with my code (which is written in R using Shiny, with JavaScript for the D3 visualization). Behind the scenes, things are supposed to be held only temporarily in your current session, but I can't guarantee that the file isn't cached by the server in one way or another. In other words, if it's sensitive data, you might not want to upload it.")))
      ),
  nav_spacer()
)

##### Server #####

server <- function(input, output) {
  the_csv <- reactive({
    if (input$load_data == "Samuel Pepys") {
      readr::read_csv("data/pepys_reciprocity-edges_extra.csv")
    } else if (input$load_data == "Star Wars") {
      readr::read_csv("data/interactions.csv")
    } else if (input$load_data == "Les Miserables") {
      readr::read_csv("data/miserables.csv")
    } else if (is.null(input$file1)) {
      readr::read_csv("data/pepys_reciprocity-edges_extra.csv") |>
        select(-date)
    } else {
      file <- input$file1
      ext <- tools::file_ext(file$datapath)

      req(file)
      validate(need(ext == "csv", "Please upload a csv file"))

      readr::read_csv(file$datapath)
    }
  })

  observe({
    updateSelectInput(
      inputId = "source",
      choices = colnames(the_middle()),
      selected =
        if (input$source %in% colnames(the_middle())) {
          input$source
        } else if ("source" %in% colnames(the_middle())) {
          "source"
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
        } else {
          ""
        })

    updateSelectInput(
      inputId = "separate_col",
      choices = colnames(the_middle()),
      selected =
        if (input$separate_col %in% colnames(the_csv())) {
          input$separate_col
        } else {
          ""
        })

    updateSelectInput(
      inputId = "split_col",
      choices = colnames(the_middle()),
      selected =
        if (input$split_col %in% colnames(the_csv())) {
          input$split_col
        } else {
          ""
        })

    updateSelectInput(
      inputId = "combine_col1",
      choices = colnames(the_middle()),
      selected =
        if (input$combine_col1 %in% colnames(the_csv())) {
          input$combine_col1
        } else {
          ""
        })

    updateSelectInput(
      inputId = "combine_col2",
      choices = colnames(the_middle()),
      selected =
        if (input$combine_col2 %in% colnames(the_csv())) {
          input$combine_col2
        } else {
          ""
        })

    # updateSelectInput(
    #   inputId = "color_col",
    #   choices = c("", colnames(the_result())))

    updateSelectInput(
      inputId = "label_col",
      choices = c("",
                  colnames(the_result()) |>
                    str_subset("target", negate = TRUE)))

    updateSelectInput(
      inputId = "size_col",
      choices = c("", colnames(the_result())),
      selected = ifelse(
        input$load_data == "Star Wars",
        "degree",
        "")
      )

    updateSelectInput(
      inputId = "weight_col",
      choices = c("", colnames(the_result())))
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

  observeEvent(input$show_label,{
    updateSelectInput(
      inputId = "label_col",
      selected =
        if (input$show_label == FALSE) {
          ""
        } else if ("source" %in% colnames(the_result())) {
          "source"
        })},
    ignoreInit = TRUE)

  observeEvent(input$show_color,{
    updateSelectInput(
      inputId = "color_col",
      choices = c("", colnames(the_result())),
      selected = case_when(
        input$show_color == FALSE ~ "",
        input$load_data == "Les Miserables" ~ "group",
        input$load_data == "Star Wars" ~ "colour",
        .default = ""
        )
      )},
    ignoreInit = TRUE)

  observeEvent(input$show_weight,{
    updateSelectInput(
      inputId = "weight_col",
      selected =
        if (input$show_weight == FALSE) {
          ""
        } else if ("weight" %in% colnames(the_result())) {
          "weight"
        })},
    ignoreInit = TRUE)

  observeEvent(c(input$load_data, input$file1),{
    updateSelectInput(
      inputId = "separate_col",
      choices = colnames(the_middle()),
      # selected =
      #   if (input$separate_col %in% colnames(the_csv())) {
      #     input$separate_col
      #   } else {
      #     ""
      #   }
      )

    updateSelectInput(
      inputId = "split_col",
      choices = colnames(the_middle()),
      # selected =
      #   if (input$split_col %in% colnames(the_csv())) {
      #     input$split_col
      #   } else {
      #     ""
      #   }
    )

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

    updateCheckboxInput(inputId = "do_sna", value = FALSE)

    if (input$load_data == "Star Wars") {
      bslib::accordion_panel_open(
        id = "main_sidebar",
        value = "Customize Plot"
      )
      bslib::accordion_panel_close(
        id = "main_sidebar",
        value = "Measure"
      )
      nav_select(
        id = "whole_page",
        selected = "D3"
      )
      updateCheckboxInput(
        inputId = "show_color",
        value = TRUE)
      updateRadioButtons(
        inputId = "color_branch",
        selected = "from column")
      updateSelectInput(
        inputId = "color_col",
        selected = "colour")
      updateCheckboxInput(
        inputId = "show_label",
        label = "Label",
        value = TRUE
      )
      # updateSelectInput(
      #   inputId = "size_col",
      #   selected = "degree",
      #   choices = c("", colnames(the_result())))
    } else if (input$load_data == "Les Miserables") {
      bslib::accordion_panel_open(
        id = "main_sidebar",
        value = "Measure"
      )
      updateCheckboxInput(
        inputId = "do_sna",
        value = TRUE
      )
      updateRadioButtons(
        inputId = "choose_directedness",
        selected = "undirected"
      )
      bslib::accordion_panel_close(
        id = "main_sidebar",
        value = "Customize Plot"
      )
      nav_select(
        id = "whole_page",
        selected = "ggplot2"
      )
      updateCheckboxInput(
        inputId = "show_color",
        value = TRUE)
      # updateRadioButtons(
      #   inputId = "color_branch",
      #   selected = "from column")
      updateSelectInput(
        inputId = "color_col",
        selected = "group")
      updateCheckboxInput(
        inputId = "show_label",
        label = "Label",
        value = FALSE
      )
    } else {
      updateCheckboxInput(inputId = "show_color", value = FALSE)
    }

    if (input$load_data == "Star Wars") {
      updateSelectInput(
        inputId = "size_col",
        selected = "degree",
        choices = c("", colnames(the_result())))
    }

    updateCheckboxInput(inputId = "show_weight", value = FALSE)

    if (input$load_data == "Les Miserables") {
      updateSelectInput(
        inputId = "add_measures",
        selected = "degree"
      )
    } else {
      updateSelectInput(
        inputId = "add_measures",
        selected = "")
    }

  })

  the_middle <- reactive({
    my_middle <- the_csv()

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
        color = ifelse(
          input$color_branch == "from column",
          input$color_col,
          ""),
        degree = input$size_col,
        weight = input$weight_col) |>
      r2d3::r2d3(
        d3_version = 4,
        script = "forcegraph.js",
        options = list(
          show_labels = input$show_label,
          default_color = input$custom_col))
  })

  output$csv_contents <- renderTable({
    the_csv()
  })

  output$contents <- renderTable({
    the_result()
  })

  output$ggt <- renderTable({
    the_network()
  })

  output$ggv <- renderPlot({
    the_data <- the_network() |>
      prepare_plot_df(input)

    make_plot(the_data, input)
  })

  output$download_csv <- downloadHandler(
    filename = function() {
      if (is.null(input$file1)) {
        "pepys-reciprocity.csv"
      } else {
        input$file1
      } |>
        str_remove_all("[.].*$") |>
        paste0(".csv")
    },
    content = function(file) {
      the_result() |>
        readr::write_csv(file)
    }
  )

  output$download_json <- downloadHandler(
    filename = function() {
      if (is.null(input$file1)) {
        "pepys-reciprocity.json"
      } else {
        input$file1 |>
          str_remove_all("[.].*$") |>
          paste0(".json")
      }
    },
    content = function(file) {
      the_result() |>
        df2d3_json() |>
        jsonlite::write_json(file, pretty = TRUE)
    }
  )

  output$download_png <- downloadHandler(
    filename = function() {
      if (is.null(input$file1)) {
        "pepys-reciprocity.png"
      } else {
        input$file1
      } |>
        str_remove_all("[.].*$") |>
        paste0(".png")
    },
    content = function(file) {
      ggsave(file, width = 7, height = 7)
    }
  )
  output$download_pdf <- downloadHandler(
    filename = function() {
      if (is.null(input$file1)) {
        "pepys-reciprocity.pdf"
      } else {
        input$file1
      } |>
        str_remove_all("[.].*$") |>
        paste0(".pdf")
    },
    content = function(file) {
      ggsave(file, width = 10, height = 10)
    }

  )

  observeEvent(input$whole_page, {
    if (input$whole_page == "D3") {
      hide("layout_choice")
      hide("show_arrow")
      hide("show_curve")
      hide("show_legend")
    } else {
      show("layout_choice")
      show("show_arrow")
      show("show_curve")
      show("show_legend")
    }
  })


}

# Run the application
shinyApp(ui = ui, server = server)
