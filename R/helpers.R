##### Downloading #####

get_online_file <- function(url, localname = NULL) {
  url_pepys <- "https://raw.githubusercontent.com/jmclawson/data_networker/refs/heads/main/data/pepys_reciprocity-edges_extra.csv"
  url_starwars <- "https://raw.githubusercontent.com/evelinag/StarWars-social-network/refs/heads/master/networks/starwars-full-interactions.json"
  url_miserables <- "https://raw.githubusercontent.com/mbostock/vega/066309624c45b1ab15e0abbc295f90878b2f33a7/docs/data/miserables.json"

  if (url == url_pepys) {
    localname <- "data/pepys_reciprocity-edges_extra.csv"
  } else if (url == url_starwars) {
    localname <- "data/interactions.csv"
  } else if (url == url_miserables) {
    localname <- "data/miserables.csv"
  }

  if(is.null(localname)){
    localname <- url |>
      stringr::str_extract("[a-z A-Z 0-9 \\- _]+[.]{1,1}+[a-zA-Z]{1,4}$")
  }

  if (!file.exists(localname)) {
    url |>
      download.file(localname)
  }
  return(localname)
}

##### Converting JSON #####

network_json2df <- function(file) {
  network <- jsonlite::fromJSON(file)

  n_nodes <- nrow(network$nodes) - 1

  network$nodes$value <- network$nodes$value %||% 1
  network$nodes$group <- network$nodes$group %||% network$nodes$color %||% network$nodes$colour %||% 1
  network$links$value <- network$links$value %||% 1

  network_df <- network$nodes |>
    mutate(id = 0:n_nodes, source_id = id) |>
    relocate(id) |>
    rename(
      degree = value,
      source = name
    ) |>
    left_join(
      network$links |>
        rename(
          target_id = target,
          weight = value
        ),
      by = join_by(source_id == source)
    ) |>
    left_join(
      network$nodes |>
        mutate(target_id = 0:n_nodes) |>
        rename(target = name) |>
        select(target_id, target)
    ) |>
    select(-ends_with("id"))

  if (var(network_df$degree) == 0) {
    network_df <- select(network_df, -degree)
  }

  if (var(network_df$weight, na.rm = TRUE) == 0) {
    network_df <- select(network_df, -weight)
  }

  network_df
}

##### Load CSV or JSON #####

load_network_file <- function(filename) {
  ext <- tools::file_ext(filename)
  validate(need(tolower(ext) %in% c("csv", "json"), "Please choose a CSV or JSON file."))

  if (tolower(ext) == "csv") {
    results <- readr::read_csv(filename)
  } else if (tolower(ext) == "json") {
    results <- network_json2df(filename)
  }

  if (filename == "data/pepys_reciprocity-edges_extra.csv") {
    results <- results |>
      select(-date)
  }

  results
}

##### D3 and JSON #####

df2d3_json <- function(
    df,
    source = source,
    target = target,
    color_col = "",
    degree_col = "",
    weight_col = "") {
  the_df <- df
  if (color_col == "" || !color_col %in% colnames(the_df)) {
    the_df <- the_df |>
      mutate(color = 1)
  } else {
    the_df <- the_df |>
      mutate(color = .data[[color_col]])
  }

  if (degree_col == "" || !degree_col %in% colnames(the_df)) {
    the_df <- the_df |>
      mutate(degree = 5)
  } else {
    the_df <- the_df |>
      mutate(degree = 5 * as.integer(cut_interval(.data[[degree_col]], n = 4)))
  }

  if (weight_col == "" || !weight_col %in% colnames(the_df)) {
    the_df <- the_df |>
      mutate(weight = 1)
  } else {
    the_df <- the_df |>
      mutate(weight = .data[[weight_col]])
  }

  the_df <- the_df |>
    tidyr::drop_na({{ source }})

  missing_sources <-
    setdiff(
      the_df |>
        pull({{ target }}),
      the_df |>
        pull({{ source }}))

  if (length(missing_sources) > 0) {
    the_df <-
      full_join(
        the_df,
        data.frame(source = missing_sources)) |>
      tidyr::drop_na({{ source }})
  }

  json_nodes <-
    the_df |>
    dplyr::select(
      id = {{ source }},
      group = color,
      size = degree) |>
    dplyr::distinct()

  json_edges <-
    the_df |>
    tidyr::drop_na({{ target }}) |>
    dplyr::select(
      source = {{ source }},
      target = {{ target }},
      value = weight) |>
    dplyr::distinct()

  list(
    nodes = json_nodes,
    links = json_edges) |>
    jsonlite::toJSON() |>
    jsonlite::parse_json()
}

##### data wrangling #####

extend_column <- function(df, col, extend_str) {
  col_str <- col |>
    rlang::enquo() |>
    rlang::quo_text()
  if (col_str %in% colnames(df)) {
    df |>
      rowwise() |>
      mutate(
        {{ col }} := {{ col }} |>
          strsplit(extend_str) |>
          unlist() |>
          trimws() |>
          list()) |>
      ungroup() |>
      unnest({{ col }})
  }
}

divide_column <- function(df, col0, split, new1, new2) {
  col0_str <- col0 |>
    rlang::enquo() |>
    rlang::quo_text() |>
    str_remove_all("[`]")
  new1_str <- new1 |>
    rlang::enquo() |>
    rlang::quo_text() |>
    str_remove_all("[`]")
  new2_str <- new2 |>
    rlang::enquo() |>
    rlang::quo_text() |>
    str_remove_all("[`]")

  if (col0_str %in% colnames(df) &&
      !new1_str %in% colnames(df) &&
      !new2_str %in% colnames(df)) {
    df |>
      mutate(
        {{ new1 }} := {{ col0 }} |>
          str_remove_all(paste0(split,".*")),
        {{ new2 }} := {{ col0 }} |>
          str_remove_all(paste0(".*",split))
        )
  } else {
    df
  }
}

combine_columns <- function(df, col1, col2, combine = " ", new) {
  col1_str <- col1 |>
    rlang::enquo() |>
    rlang::quo_text() |>
    str_remove_all("[`]")
  col2_str <- col2 |>
    rlang::enquo() |>
    rlang::quo_text() |>
    str_remove_all("[`]")

  if (col1_str %in% colnames(df) &&
      col2_str %in% colnames(df)) {
    df |>
      mutate(
        {{ new }} := paste0({{ col1 }}, combine, {{ col2 }}))
  } else {
    df
  }
}
