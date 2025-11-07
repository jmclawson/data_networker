
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
      length(input$sna_add) > 0) {

    real_network <- my_result |>
      tidyr::drop_na(source, target) |>
      network::network(multiple = TRUE)

    if (input$directed == "undirected") {
      the_gmode <- "graph"
    } else {
      the_gmode <- "digraph"
    }

    node_measures <-
      data.frame(
        node = map_chr(real_network$val,
                       \(x) x$vertex.names),
        degree = if ("degree" %in% input$sna_add) {sna::degree(
          real_network,
          gmode = the_gmode)} else {""},
        betweenness = if ("betweenness" %in% input$sna_add) {sna::betweenness(
          real_network,
          gmode = the_gmode)} else {""},
        closeness = if ("closeness" %in% input$sna_add) {sna::closeness(
          real_network,
          gmode = the_gmode)} else {""},
        gil_schmidt = if ("gil_schmidt" %in% input$sna_add) {sna::gilschmidt(
          real_network,
          gmode = the_gmode)} else {""},
        prestige = if ("prestige" %in% input$sna_add) {sna::prestige(
          real_network,
          gmode = the_gmode)} else {""},
        stress_centrality = if ("stress_centrality" %in% input$sna_add) {sna::stresscent(
          real_network,
          gmode = the_gmode)} else {""}
      )

    my_result <- my_result |>
      left_join(
        node_measures |>
          select(
            source = node,
            all_of(input$sna_add)),
        by = "source")
  }

  my_result
}

get_network_df <- function(
    df,
    input = list(
      weight_from = "count",
      arrow = TRUE,
      layout = "fruchtermanreingold")) {
  the_df <- df |>
    relocate(source, target)

  if (input$weight_from == "count") {
    if (!"the_count" %in% colnames(df)) {
      the_df <- the_df |>
        mutate(the_count = n(),
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
      arrow.gap = ifelse(input$arrow, 0.025, 0),
      layout = input$layout#, "fruchtermanreingold",
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

  if (!input$color_col %in% c("", "source")) {
    the_data <- the_data |>
      mutate(color_groups = get(input$color_col))
  } else if (input$color_col == "source") {
    the_data <- the_data |>
      mutate(color_groups = vertex.names)
  }

  if (input$size_col != "") {
    the_data <- the_data |>
      mutate(
        size_class = !!sym(input$size_col),
        text_size = 4 + 3 * as.integer(cut_interval(!!sym(input$size_col), n = 5))
        )
  } else {
    the_data <- the_data |>
      mutate(
        text_size = 4
      )
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
      "the_count" %in% colnames(df)) {
    df <- df |>
      rename(the_weight = the_count)
  }
  print(colnames(df))

  # df$text_size[is.na(df$text_size)] <- 4

  my_plot <- df |>
    ggplot(aes(
      x = x, y = y,
      xend = xend, yend = yend))


  if (input$arrow) {
    if (input$weighted) {
      my_plot <- my_plot +
        geom_edges(
          aes(linewidth = the_weight),
          color = "grey50",
          arrow = arrow(),
          curvature = input$curve) +
        labs(linewidth = ifelse(input$weight_col != "", input$weight_col, "weight"))
    } else {
      my_plot <- my_plot +
        geom_edges(
          color = "grey50",
          arrow = arrow(),
          curvature = input$curve)
    }
  } else {
    if (input$weighted) {
      my_plot <- my_plot +
        geom_edges(
          aes(linewidth = the_weight),
          color = "grey50",
          curvature = input$curve) +
        labs(linewidth = input$weight_col)
    } else {
      my_plot <- my_plot +
        geom_edges(
          color = "grey50",
          curvature = input$curve)
    }
  }

  if (input$color_branch == "from column" &&
      input$color_col != "") {
    if (is.numeric(df$color_groups)) {
      df$color_groups <- factor(df$color_groups)
    }

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

  } else if (input$color &&
             input$color_branch == 'custom') {
    if (input$size_col != "") {
      my_plot <- my_plot +
        geom_nodes(
          aes(size = size_class),
          color = input$color_cus) +
        labs(
          size = input$size_col)
    } else {
      my_plot <- my_plot +
        geom_nodes(
          size = 4,
          color = input$color_cus)
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

  if (!input$color) {
    # no color
    if (!input$label_col %in% c("", "source")) {
      # label column is chosen
      my_plot <- my_plot +
        geom_shadowtext(
          data = df |>
            filter(!is.na(size_class)),
          aes(
            label = the_label),
          color = "black",
          bg.color = "white",
          show.legend = FALSE)
    } else if (input$label_col == "source") {
      # label column is source
      if (input$size_col != "") {
        # size column is chosen
        my_plot <- my_plot +
          geom_shadowtext(
            data = df |>
              filter(!is.na(size_class)),
            aes(
              label = vertex.names,
              size = text_size),
            color = "black",
            bg.color = "white",
            show.legend = FALSE)
      } else {
        # size column isn't chosen
        my_plot <- my_plot +
          geom_shadowtext(
            data = if ("the_count" %in% colnames(df)) {
              df |>
                filter(!is.na(the_count))
            } else {
              df |>
                filter(!is.na(x))
            },
            aes(label = vertex.names),
            color = "black",
            bg.color = "white",
            show.legend = FALSE)
      }
    }
  } else if (input$color_branch != "from column") {
    # custom color set for everything
    if (!input$label_col %in% c("", "source")) {
      # 1 custom color and label column is chosen
      my_plot <- my_plot +
        geom_shadowtext(
          data = df |>
            filter(!is.na(size_class)),
          aes(
            label = the_label),
          color = input$color_cus,
          bg.color = "white",
          show.legend = FALSE)
    } else if (input$label_col == "source") {
      # 1 custom color and label column is source
      if (input$size_col != "") {
        # size column is chosen
        my_plot <- my_plot +
          geom_shadowtext(
            data = df |>
              filter(!is.na(size_class)),
            aes(
              label = vertex.names,
              size = text_size),
            color = input$color_cus,
            bg.color = "white",
            show.legend = FALSE)
      } else {
        # size isn't variable
        my_plot <- my_plot +
          geom_shadowtext(
            data = df |>
              filter(!is.na(the_count)),
            aes(label = vertex.names),
            color = input$color_cus,
            bg.color = "white",
            show.legend = FALSE)
      }
    }
  } else {
    # color set from column
    if (!input$label_col %in% c("", "source")) {
      # color set from column and label column is chosen
      my_plot <- my_plot +
        geom_shadowtext(
          data = df |>
            filter(!is.na(size_class)),
          aes(
            label = the_label,
            color = color_groups),
          bg.color = "white",
          show.legend = FALSE)
    } else if (input$label_col == "source" && "color_groups" %in% colnames(df)) {
      # color set from column and label column is source
      if (input$size_col != "") {
        # size column is chosen
        my_plot <- my_plot +
          geom_shadowtext(
            data = df |>
              filter(!is.na(size_class)),
            aes(
              label = vertex.names,
              size = text_size,
              color = color_groups),
            bg.color = "white",
            show.legend = FALSE)
      } else {
        # size column is not chosen
        my_plot <- my_plot +
          geom_shadowtext(
            data = if ("the_count" %in% colnames(df)) {
              filter(df, !is.na(the_count))
            } else {
              df
            },
            aes(
              label = vertex.names,
              color = color_groups),
            bg.color = "white",
            show.legend = FALSE)
      }
    }
  }

  if ("color_groups" %in% colnames(df)) {
    if (substr(df$color_groups[1], 1, 1) == "#") {
      my_plot <- my_plot +
        scale_color_identity() +
        guides(color = "none")
    }
  }

  my_plot <- my_plot +
    theme_blank()

  if (!input$legend) {
    my_plot <- my_plot +
      theme(legend.position = "none")
  }

  my_plot
}
