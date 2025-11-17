sidebar_contents <- sidebar(
  id = "s",
  open = list(
    # desktop = "always",
    mobile = "closed"
  ),
  accordion(
    id = "a",
    multiple = FALSE,
    accordion_panel(
      "Import",
      icon = bsicons::bs_icon("cloud-arrow-up"),
      radioButtons(
        "data_source", "",
        inline = TRUE,
        choices = c("URL", "upload")),
      conditionalPanel(
        condition = "input.data_source == 'URL'",
        textAreaInput(
          "u", "",
          placeholder = "")#https://raw.githubusercontent.com/jmclawson/data_networker/refs/heads/main/data/pepys_reciprocity-edges_extra.csv")
      ),
      conditionalPanel(
        condition = "input.data_source == 'upload'",
        fileInput(
          "file", "",
          accept = c(".csv", ".json"))
      ),
      shiny::checkboxInput( # TODO: Add support for separated edge and network data
        "separate_nodes", "Separate nodes file"),
      conditionalPanel(
        "input.separate_nodes",
        radioButtons(
          "data_source2", "",
          inline = TRUE,
          choices = c("URL", "upload")),
        conditionalPanel(
          condition = "input.data_source2 == 'URL'",
          textAreaInput(
            "u2", "",)
        ),
        conditionalPanel(
          condition = "input.data_source2 == 'upload'",
          fileInput(
            "file2", "",
            accept = c(".csv", ".json"))
        ),
        uiOutput("set_match_cols", inline = TRUE)
        ),
      actionButton("load_button", "Load")
    ),
    accordion_panel(
      "Adjust",
      icon = bsicons::bs_icon("dpad"),
      checkboxInput(
        "do_separate",
        label = span(bsicons::bs_icon("arrows-expand"), "Separate one column's values into multiple rows."),
        value = FALSE),
      conditionalPanel(
        condition = "input.do_separate",
        wellPanel(
          selectInput(
            "separate_col", "Column",
            choices = c("")),
          textInput(
            "separate", "Separate at",
            placeholder = "(e.g., comma, underscore, space)")
        ),
        p()
      ),
      checkboxInput(
        "do_split",
        label = span(bsicons::bs_icon("arrows-expand-vertical"), "Split one column into two."),
        value = FALSE),
      conditionalPanel(
        condition = "input.do_split",
        wellPanel(
          selectInput(
            "split_col", "Column",
            choices = c("")),
          textInput(
            "split", "Split at",
            placeholder = "(e.g., comma, underscore, space)"),
          textInput(
            "split_col1", "New column name 1",
            value = "col1"),
          textInput(
            "split_col2", "New column name 2",
            value = "col2")
        ),
        p()
      ),
      checkboxInput(
        "do_combo",
        label = span(bsicons::bs_icon("arrows-collapse-vertical"), "Combine two columns into one."),
        value = FALSE),
      conditionalPanel(
        condition = "input.do_combo",
        wellPanel(
          selectInput(
            "combine_col1", "Column 1",
            choices = c("")),
          selectInput(
            "combine_col2", "Column 2",
            choices = c("")),
          textInput(
            "combine", "Combine at",
            placeholder = "(e.g., comma, underscore, space)"),
          textInput(
            "combine_name", "Name",
            value = "combo"))
      )
    ),
    accordion_panel(
      "Choose",
      id = "enforce_columns",
      icon = bsicons::bs_icon("list-check"),
      selectInput(
        "source", "Source column",
        choices = c("source")),
      selectInput(
        "target", "Target column",
        choices = c("target"))
    ),
    accordion_panel(
      "Measure",
      icon = bsicons::bs_icon("node-plus"),
      checkboxInput(
        "do_sna", "Add network measurements.",
        value = FALSE),
      conditionalPanel(
        condition = "input.do_sna",
        wellPanel(
          radioButtons(
            "directed", "Graph mode",
            choices = c(
              "undirected",
              "directed")),
          selectInput(
            "sna_add", "Choose measurements",
            multiple = TRUE,
            choices = c("degree", "betweenness", "closeness", "prestige", "stress_centrality")),
          p("Degree reports total (in- and out-degree) values. All measurements use the", a(href = "https://cran.r-project.org/web/packages/sna/index.html", "sna"), "package.")
        )
      )
    ),
    accordion_panel(
      "Customize",
      icon = bsicons::bs_icon("palette"),
      selectInput(
        "layout", "Network layout",
        choices = c(
          "circle", "eigen", "fruchtermanreingold",
          "hall", "kamadakawai", "mds",
          "princoord", "random", "spring",
          "target"),
        selected = "fruchtermanreingold"),
      checkboxInput(
        "arrow", "Directed arrows",
        value = FALSE),
      checkboxInput(
        "weighted", "Weighted edges",
        value = FALSE),
      conditionalPanel(
        condition = "input.weighted",
        wellPanel(
          radioButtons(
            "weight_from", "Defined by",
            choices = c("count", "column"),
            selected = "count"),
          conditionalPanel(
            condition = "(input.weight_from == 'column') || (input.weighted && input.p == 'D3')",
            selectInput(
              "weight_col", "",
              choices = c(""))
          )
        ),
        p()
      ),
      checkboxInput(
        "color", "Colored nodes",
        value = FALSE),
      conditionalPanel(
        condition = "input.color",
        wellPanel(
          radioButtons(
            "color_branch", "Color source:",
            selected = "custom",
            choices = c("custom", "from column")),
          conditionalPanel(
            condition = "input.color_branch == 'custom'",
            colourpicker::colourInput(
              "color_cus",
              label = NULL,
              value = "#6c75adaa",
              allowTransparent = TRUE,
              closeOnClick = TRUE)
          ),
          conditionalPanel(
            condition = "input.color_branch == 'from column'",
            selectInput(
              "color_col", "",
              choices = c("")))
          ),
        p()
      ),
      checkboxInput(
        "label", "Labeled nodes",
        value = FALSE),
      conditionalPanel(
        condition = "input.label",
        wellPanel(
          selectInput(
            "label_col", "Column:",
            choices = c(""))),
        p()),
      selectInput(
        "size_col", "Node size by column",
        choices = c("")),
      numericInput(
        "curve", "Curviness",
        value = 0.1, step = 0.1),
      checkboxInput(
        "legend", "Legend",
        value = TRUE),
      card(p(a(href = "https://ggplot2.tidyverse.org", "ggplot2"), "visualization uses", a(href = "https://briatte.github.io/ggnetwork/", "ggnetwork"), "to calculate geometries for nodes and edges. Network layouts listed here are from", a(href = "https://cran.r-project.org/web/packages/sna/index.html", "sna"), "."))
    ),
    p(
    wellPanel(
      # tags$h6(bsicons::bs_icon("cloud-download"),
      #         "Export"),
      uiOutput("share_url"),
      div(
        downloadLink(
          "download_csv",
          list(
            icon("file-csv", class = "export-links"),
            "CSV for Gephi"),
          title = "attributed edge table"
          )),
      div(
        downloadLink(
          "download_json",
          list(
            icon("file", class = "export-links"),
            "JSON for D3"),
          title = "nodes and edges"
        )),
      div(
        downloadLink(
          "download_png",
          list(
            icon("file-image", class = "export-links"),
            "PNG from ggplot2"),
          title = "static visualization"
        )),
      div(
        downloadLink(
          "download_pdf",
          list(
            icon("file-pdf", class = "export-links"),
            "PDF from ggplot2"),
          title = "scaleable visualization"
        ))
    ))
  )
)

tables_contents <- nav_panel(
  title = "Tables",
  icon = icon("table"),
  shinyjs::useShinyjs(),
  card(
    id = "original",
    card_header("Loaded data"),
    max_height = 300,
    full_screen = TRUE,
    DT::dataTableOutput("df_contents")
  ),
  fixedRow(
    layout_columns(
      col_widths = c(9, 3),
      card(
        max_height = 380,
        full_screen = TRUE,
        tabsetPanel(
          tabPanel(
            title = "Adjusted data",
            DT::dataTableOutput("contents")
          ),
          tabPanel(
            title = "Geometry for ggplot2",
            DT::dataTableOutput("ggt")
          )
        )
      ),
      column(
        12,
        fluidRow(
          value_box(
            title = "Nodes",
            max_height = (380 / 3) - 2,
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
            max_height = (380 / 3) - 3,
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
            max_height = (380 / 3) - 3,
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
)

ggplot2_contents <- nav_panel(
  title = "ggplot2",
  value = "ggplot2",
  icon = icon("circle-nodes"),
  plotOutput("ggv",
    height = "90vh"
  )
)

d3_contents <- nav_panel(
  title = "D3",
  value = "D3",
  icon = icon("square-js"),
  d3Output("d3",
    height = "90vh"
  ),
  full_screen = TRUE
)

notes_contents <- nav_panel(
  title = "Notes",
  icon = icon("circle-info"),
  layout_columns(
    card(
      card_header("Networking data"),
      p(
        "Many datasets work with the tool, both for adding network measurements and visualizing connections. Here are three:",
        tags$dl(
          tags$dt(a(href="https://jmclawson.shinyapps.io/data_networker/?a=Customize&p=ggplot2&u=https://raw.githubusercontent.com/jmclawson/data_networker/refs/heads/main/data/pepys_reciprocity-edges_extra.csv&arrow=1&color=1&label=1&do_sna=1&legend=0&sna_add=degree&directed=directed&size_col=degree&weighted=1&color_col=gender&color_branch=from%20column&label_col=source", "Pepys reciprocity data")),#uiOutput("link_pepys", inline = TRUE)),
          tags$dd(
            "Pepys data from",
            em(a(href = "https://www.pepysdiary.com", "The Diary of Samuel Pepys")),
            "showing reported reciprocity of social favors and gifts in the first week of April 1667. It was collected from the diary by Paula Chan, James Clawson, Caroline Greer, Joseph Stuart, and Sarah Tew as part of a",
            a(href = "https://mathhumanists.org", "Mathematical Humanists"),
            "workshop led by Jessica Otis and Ashley Sanders."
          ),
          tags$dt(HTML("<a href='https://jmclawson.shinyapps.io/data_networker/?a=Customize&p=D3&s=0&u=https://raw.githubusercontent.com/evelinag/StarWars-social-network/refs/heads/master/networks/starwars-full-interactions.json&color=1&label=1&size_col=degree&weighted=1&color_col=group&weight_col=weight&color_branch=from%20column&label_col=source'><i>Star Wars</i> Characters</a>")),#uiOutput("link_star_wars", inline = TRUE)),
          tags$dd(
            "Evelina Gabasova's", em("Star Wars"), "data is particularly worth exploring. Gabasova explains the data collection in a",
            a(href = "https://evelinag.com/blog/2015/12-15-star-wars-social-network/index.html#how", "blog post"),
            "and shares a",
            HTML("<a href='https://github.com/evelinag/StarWars-social-network/blob/master/networks/starwars-full-interactions.json'>JSON file on GitHub</a>.")
          ),
          tags$dt(HTML("<a href='https://jmclawson.shinyapps.io/data_networker/?a=Measure&p=ggplot2&u=https://raw.githubusercontent.com/mbostock/vega/066309624c45b1ab15e0abbc295f90878b2f33a7/docs/data/miserables.json&color=1&label=1&do_sna=1&legend=0&directed=undirected&size_col=degree&color_col=group&color_branch=from%20column&label_col=source&sna_add=degree'><i>Les Misérables</i> Characters</a>")),#uiOutput("link_les_miserables", inline = TRUE)),
          tags$dd(
            "Also commonly studied in network analysis, Donald Knuth's", em("Les Misérables"), "data from his work on the Stanford Graph Base, can be found in a version from the",
            a(href = "https://github.com/mbostock/vega/blob/066309624c45b1ab15e0abbc295f90878b2f33a7/docs/data/miserables.json", "JSON file"),
            "made available by",
            HTML("<a href='https://bost.ocks.org/mike/miserables/'>Mike Bostock</a>."),
            "Bostock also uses this dataset to demonstrate",
            HTML("<a href='https://observablehq.com/@d3/force-directed-graph/2'>JavaScript code</a>"),
            "for visualizing force-directed network layouts with D3."
          )
        )
      )
    ),
    card(
      card_header("Performance"),
      p("This page is hosted on a free shinyapps.io account with limitations on time and processing power, so don't be alarmed if it gets slow. I've also", a(href = "https://github.com/jmclawson/data_networker", "shared the source code"), "if you'd like to run it on your own machine, which is much faster than running on a server over the Internet."),
      p("A note on privacy: I can't see what you're uploading, but I do have access to logs that show when there's a problem with my code (which is written in R using Shiny, with JavaScript for the D3 visualization). Behind the scenes, things are supposed to be held only temporarily in your current session, but I can't guarantee that the file isn't cached by the server in one way or another. In other words, if it's sensitive data, you might not want to upload it.")
    )
  )
)
