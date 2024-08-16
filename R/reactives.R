
get_result_df <- function(df, input) {
  my_result <- df

  if (input$source %in% colnames(my_result) &&
      input$source != "source") {
    if ("source" %in% colnames(my_result)) {
      names(my_result)[names(my_result) == "source"] <- "source_old"
    }
    names(my_result)[names(my_result) == input$source] <- "source"
  }

  if (input$target %in% colnames(my_result) &&
      input$target != "target") {
    if ("target" %in% colnames(my_result)) {
      names(my_result)[names(my_result) == "target"] <- "target_old"
    }
    names(my_result)[names(my_result) == input$target] <- "target"
  }

  if ("source" %in% colnames(my_result) &&
      "target" %in% colnames(my_result)) {
    my_result <- my_result |>
      relocate(source, target)
  }

  if (input$do_sna) {
    validate(
      need(
        "source" %in% colnames(my_result) &&
          "target" %in% colnames(my_result), "Valid `source` and `target` columns are needed for calculations."))
  }

  if (input$do_sna &&
      length(input$add_measures) > 0) {

    real_network <- my_result |>
      tidyr::drop_na(source, target) |>
      network::network(multiple = TRUE)

    if (input$choose_directedness == "undirected") {
      the_gmode <- "graph"
    } else {
      the_gmode <- "digraph"
    }

    node_measures <-
      data.frame(
        node = map_chr(real_network$val,
                       \(x) x$vertex.names),
        degree = sna::degree(
          real_network,
          gmode = the_gmode),
        betweenness = sna::betweenness(
          real_network,
          gmode = the_gmode),
        closeness = sna::closeness(
          real_network,
          gmode = the_gmode),
        gil_schmidt = sna::gilschmidt(
          real_network,
          gmode = the_gmode),
        prestige = sna::prestige(
          real_network,
          gmode = the_gmode),
        stress_centrality = sna::stresscent(
          real_network,
          gmode = the_gmode)
      )

    my_result <- my_result |>
      left_join(
        node_measures |>
          select(
            source = node,
            all_of(input$add_measures)),
        by = "source")
  }

  my_result
}

get_network_df <- function(
    df,
    input = list(
      weight_from = "count",
      show_arrow = TRUE,
      layout_choice = "fruchtermanreingold")) {
  the_df <- df |>
    relocate(source, target)

  if (input$weight_from == "count") {
    if (!"count" %in% colnames(df)) {
      the_df <- the_df |>
        mutate(count = n(),
               .by = c(source, target))
    } else {
      the_df <- the_df |>
        mutate(the_weight = n(),
               .by = c(source, target))
    }
  }

  my_network1 <- the_df |>
    tidyr::drop_na(source, target) |>
    slice_head(n = 1,
               by = c(source, target))

  my_network <- my_network1 |>
    ggnetwork(
      arrow.gap = ifelse(input$show_arrow, 0.025, 0),
      layout = input$layout_choice#, "fruchtermanreingold",
      # weights = "weight"
    )

  missing <- df |>
    filter(is.na(target)) |>
    rename(vertex.names = source) |>
    select(-target) |>
    slice_head(
      n = 1,
      by = vertex.names) |>
    select(where(\(x) sum(is.na(x)) == 0))

  my_network <- my_network |>
    rows_patch(
      missing,
      by = "vertex.names",
      unmatched = "ignore") |>
    rows_insert(
      missing,
      by = "vertex.names",
      conflict = "ignore")

  my_network
}

##### Plotting #####

prepare_plot_df <- function(df, input) {
  the_data <- df

  if (input$color_col != "") {
    the_data <- the_data |>
      mutate(color_groups = get(input$color_col))
  }

  if (input$size_col != "") {
    the_data <- the_data |>
      mutate(size_class = get(input$size_col))
  }

  if (input$weight_col != "" &&
      input$weight_from == "column") {
    the_data <- the_data |>
      mutate(the_weight = get(input$weight_col))
  }

  if (!input$label_col %in% c("", "source")) {
    the_data <- the_data |>
      mutate(the_label = get(input$label_col))
  }

  the_data
}

make_plot <- function(df, input) {
  if (!"the_weight" %in% colnames(df) &&
      "count" %in% colnames(df)) {
    df <- df |>
      rename(the_weight = count)
  }

  my_plot <- df |>
    ggplot(aes(
      x = x, y = y,
      xend = xend, yend = yend))


  if (input$show_arrow) {
    if (input$show_weight) {
      my_plot <- my_plot +
        geom_edges(
          aes(linewidth = the_weight),
          color = "grey50",
          arrow = arrow(),
          curvature = input$show_curve) +
        labs(linewidth = ifelse(input$weight_col != "", input$weight_col, "weight"))
    } else {
      my_plot <- my_plot +
        geom_edges(
          color = "grey50",
          arrow = arrow(),
          curvature = input$show_curve)
    }
  } else {
    if (input$show_weight) {
      my_plot <- my_plot +
        geom_edges(
          aes(linewidth = the_weight),
          color = "grey50",
          curvature = input$show_curve) +
        labs(linewidth = input$weight_col)
    } else {
      my_plot <- my_plot +
        geom_edges(
          color = "grey50",
          curvature = input$show_curve)
    }
  }

  if (input$color_branch == "from column" &&
      input$color_col != "") {
    if (input$size_col != "") {
      my_plot <- my_plot +
        geom_nodes(
          data = df |>
            tidyr::drop_na(color_groups),
          aes(color = color_groups,
              size = size_class)) +
        labs(
          color = input$color_col,
          size = input$size_col)
    } else {
      my_plot <- my_plot +
        geom_nodes(
          data = df |>
            tidyr::drop_na(color_groups),
          aes(color = color_groups),
          size = 4) +
        labs(color = input$color_col)
    }
  } else if (input$show_color &&
             input$color_branch == 'custom') {
    if (input$size_col != "") {
      my_plot <- my_plot +
        geom_nodes(
          aes(size = size_class),
          color = input$custom_col) +
        labs(
          size = input$size_col)
    } else {
      my_plot <- my_plot +
        geom_nodes(size = 4,
                   color = input$custom_col)
    }
  } else {
    if (input$size_col != "") {
      my_plot <- my_plot +
        geom_nodes(
          aes(size = size_class)) +
        labs(
          size = input$size_col)
    } else {
      my_plot <- my_plot +
        geom_nodes(size = 4)
    }
  }

  if (!input$label_col %in% c("", "source")) {
    my_plot <- my_plot +
      geom_text(
        aes(label = the_label))
  } else if (input$label_col == "source") {
    my_plot <- my_plot +
      geom_text(
        aes(label = vertex.names))
  }

  my_plot <- my_plot +
    theme_blank()

  if (!input$show_legend) {
    my_plot <- my_plot +
      theme(legend.position = "none")
  }

  my_plot
}
