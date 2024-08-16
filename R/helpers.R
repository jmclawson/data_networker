
##### D3 and JSON #####

df2d3_json <- function(
    df,
    source = source,
    target = target,
    group = group,
    weight = weight) {
  the_df <- df
  if (!deparse(substitute(group)) %in% colnames(the_df)) {
    the_df <- the_df |>
      mutate({{ group }} := 1)
  }

  if (!deparse(substitute(weight)) %in% colnames(the_df)) {
    the_df <- the_df |>
      mutate({{ weight }} := 1)
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
      group = {{ group }}) |>
    dplyr::distinct()

  json_edges <-
    the_df |>
    tidyr::drop_na({{ target }}) |>
    dplyr::select(
      source = {{ source }},
      target = {{ target }},
      value = {{ weight }}) |>
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

combine_columns <- function(df, col1, col2, new) {
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
        {{ new }} := paste({{ col1 }}, {{ col2 }}))
  } else {
    df
  }
}
