library(tidyverse)
library(shiny)
library(bslib)
library(r2d3)
library(network)
library(sna)
library(ggnetwork)
library(colourpicker)
source("R/helpers.R")
source("R/reactives.R")


##### UI #####
ui <- page_sidebar(
  # title = "data networker",
  theme = bs_theme(
    "navbar-bg" = "#e8e8e8",
    primary = "#6c75ad",
    version = 5,
    bootswatch = "bootstrap",
    ),
  tags$head(tags$style(
    HTML(
      "
        .bslib-full-screen-enter {
          bottom: var(--bslib-full-screen-enter-bottom);
        }
      "
    )
  )),
  sidebar = accordion(
    multiple = FALSE,
    accordion_panel(
      "Import Data",
      icon = bsicons::bs_icon("cloud-arrow-up"),
      fileInput("file1", "Choose CSV File", accept = ".csv")),
    accordion_panel(
      "Mend",
      icon = bsicons::bs_icon("bandaid"),
      checkboxInput("do_split",
                    label = "Split one column's values into multiple rows.",
                    value = FALSE
      ),
      conditionalPanel(
        condition = "input.do_split",
        selectInput("split_col",
                    "Column",
                    choices = c("")),
        textInput("split", "Split at")),
      checkboxInput("do_combo",
                    label = "Combine two columns into one.",
                    value = FALSE
      ),
      conditionalPanel(
        condition = "input.do_combo",
        selectInput("combine_col1",
                    "Column 1",
                    choices = c("")),
        selectInput("combine_col2",
                    "Column 2",
                    choices = c("")),
        textInput("combine_name", "Name", value = "combo")
      )),
    accordion_panel(
      "Choose", id = "enforce_columns",
      icon = bsicons::bs_icon("list-stars"),
      selectInput("source", "Source column", choices = c("source")),
      selectInput("target", "Target column", choices = c("target"))),
    accordion_panel(
      "Measure",
      icon = bsicons::bs_icon("node-plus"),#rocket-takeoff node-plus magic layout-wtf columns-gap hypnotize clipboard2-plus bezier bezier2
      checkboxInput("do_sna",
                    label = "Add network measurements.",
                    value = FALSE
      ),
      conditionalPanel(
        condition = "input.do_sna",
        radioButtons("choose_directedness",
                     "Graph mode",
                     choices = c("directed",
                                 "undirected")),
        selectInput("add_measures",
                    label = "Choose measurements",
                    multiple = TRUE,
                    choices = c("degree", "betweenness", "closeness", "prestige", "stress_centrality")
        ),
        p("Degree reports total (in- and out-degree) values. All measurements use",
          a(href = "https://cran.r-project.org/web/packages/sna/index.html", "sna"), "."),
      )
    ),
    accordion_panel(
      "Export",
      icon = bsicons::bs_icon("cloud-download"),
      downloadLink("download_csv", "Gephi edges file"),
      downloadLink("download_json", "JSON file for D3"),
      downloadLink("download_png", "Plot as PNG")),
    accordion_panel(
      "Customize Plot",
      icon = bsicons::bs_icon("palette"),#diagram-3
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
      checkboxInput("show_color",
                    label = "Color",
                    value = FALSE
      ),
      conditionalPanel(
        condition = "input.show_color",
        radioButtons(
          inputId = "color_branch",
          label = "Choose color source",
          selected = "custom",
          choices = c("custom", "from column")),
        conditionalPanel(
          condition = "input.color_branch == 'custom'",
          colourInput(
            "custom_col",
            label = NULL,
            value = "#6c75adaa",
            allowTransparent = TRUE,
            closeOnClick = TRUE)
        ),
        conditionalPanel(
          condition = "input.color_branch == 'from column'",
          selectInput("color_col",
                      "Color points by column",
                      choices = c(""))
        )
      ),
      checkboxInput("show_label",
                    label = "Label",
                    value = FALSE
      ),
      conditionalPanel(
        condition = "input.show_label",
        selectInput("label_col",
                    "Label by values in column",
                    choices = c(""))
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
      p(a(href = "https://ggplot2.tidyverse.org", "ggplot2"), "visualization uses", a(href = "https://briatte.github.io/ggnetwork/", "ggnetwork"), "to calculate geometries for nodes and edges. Network layouts listed here are from", a(href = "https://cran.r-project.org/web/packages/sna/index.html", "sna"), ".")
    )
    ),
  tabsetPanel(
    tabPanel(
      title = "Explore tables",
      card(id = "original",
           card_header("original CSV data"),
           max_height = 300,
           full_screen = TRUE,
           tableOutput("csv_contents")),
      layout_columns(
        card(id = "adjusted",
             card_header("adjusted"),
             max_height = 450,
             full_screen = TRUE,
             tableOutput("contents")),
        card(card_header("geometry for ggplot2"),
             max_height = 450,
             tableOutput("ggt")))),
    tabPanel(title = "Visualize with ggplot2",
             plotOutput("ggv",
                        height = "90vh")),
    tabPanel(
      title = "Visualize with D3",
      d3Output("d3",
               height = "90vh"),
      full_screen = TRUE),
    tabPanel(
      title = "Notes",
      layout_columns(
        uiOutput("num_components"),
        uiOutput("num_nodes"),
        uiOutput("num_edges"),
        uiOutput("num_connectedness")
      ),
      layout_columns(
        card(
          card_header("Methods"),
          p("The network calculations here don't yet normalize values between 0 and 1. Additionally, I haven't compared results to Gephi, so you should probably be consistent in the tools you use for calculating these values. (That said,", a(href = "https://cran.r-project.org/web/packages/sna/index.html", "sna"), "is a trusted library with more than 2 million downloads from CRAN, so it's probably dependable even if it calculates things differently than Gephi.)"),
          p("There's not much customization available for the D3 visualization, and I don't plan to add any. D3 nodes might seem to go missing when data changes. They're still there! Find them tucked away trying to hide in the upper-left corner of the page. They'll shuffle back into place when you click them. Alternatively, try toggling the sidebar."),
          p("Sample data from", em(a(href = "https://www.pepysdiary.com", "The Diary of Samuel Pepys")), "showing reported reciprocity of social favors and gifts in the first week of April 1667. It was collected from the diary by Paula Chan, James Clawson, Caroline Greer, Joseph Stuart, and Sarah Tew as part of a", a(href="https://mathhumanists.org", "Mathematical Humanists"), "workshop led by Jessica Otis and Ashley Sanders.")),
      card(
        card_header("shinyapps.io"),
        p("This page is hosted on a free account with limitations on time and processing power, so please don't spread the link too widely. If it's useful enough to keep around, I'll move it somewhere more sustainable. I've also", a(href = "https://github.com/jmclawson/data_networker", "shared the source code"), "if you'd like to run it on your own machine, which is much faster than running on a server over the Internet."),
        p("A note on privacy: I can't see what you're uploading, but I do have access to logs that show when there's a problem with my code (which is written in R using Shiny). Behind the scenes, things are supposed to be held only temporarily in your current session, but I can't guarantee that the file isn't cached by the server in one way or another. In other words, if it's sensitive data, you might not want to upload it.")))
      )
  )
)

##### Server #####

server <- function(input, output) {
  the_csv <- reactive({
    if (is.null(input$file1)) {
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

    updateSelectInput(
      inputId = "color_col",
      choices = c("", colnames(the_result())))

    updateSelectInput(
      inputId = "label_col",
      choices = c("",
                  colnames(the_result()) |>
                    str_subset("target", negate = TRUE)))

    updateSelectInput(
      inputId = "size_col",
      choices = c("", colnames(the_result())))

    updateSelectInput(
      inputId = "weight_col",
      choices = c("", colnames(the_result())))
  })

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
      selected = if(input$show_color == FALSE){""})},
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

  observeEvent(input$file1,{
    updateSelectInput(
      inputId = "split_col",
      choices = colnames(the_middle()),
      selected =
        if (input$split_col %in% colnames(the_csv())) {
          input$split_col
        } else {
          ""
        })

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
      inputId = "split_col",
      selected = "")

    updateSelectInput(
      inputId = "size_col",
      selected = "",
      choices = c("", colnames(the_result())))

    updateCheckboxInput(inputId = "do_split", value = FALSE)

    updateCheckboxInput(inputId = "do_combo", value = FALSE)

    updateCheckboxInput(inputId = "do_sna", value = FALSE)

    updateCheckboxInput(inputId = "show_color", value = FALSE)

    updateCheckboxInput(inputId = "show_weight", value = FALSE)

    updateSelectInput(
      inputId = "add_measures",
      selected = "")

  })

  the_middle <- reactive({
    my_middle <- the_csv()

    if (input$do_combo) {
      my_middle <- my_middle |>
        combine_columns(!!sym(input$combine_col1),
                        !!sym(input$combine_col2),
                        !!sym(input$combine_name))
    }

    if (nchar(input$split) > 0) {
      my_middle <- my_middle |>
        extend_column(!!sym(input$split_col), input$split)
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

  output$d3 <- renderD3({
    validate(
      need(input$source %in% colnames(the_middle()), "Please choose a valid `source` column."),
      need(input$target %in% colnames(the_middle()), "Please choose a valid `target` column.")
    )
    the_result() |>
      df2d3_json() |>
      r2d3::r2d3(d3_version = 4, script = "forcegraph.js")
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
    filename = ifelse(is.null(input$file1),
                      "pepys-reciprocity.png",
                      input$file1 |>
                        str_remove_all("[.].*$") |>
                        paste0(".png")),
    content = function(file) {
      ggsave(file)
    }
  )


}

# Run the application
shinyApp(ui = ui, server = server)
