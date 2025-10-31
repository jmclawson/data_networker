flatten_network <- function(file) {
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

if (!file.exists("data/interactions.csv")) {
  if (!file.exists("data/starwars-full-interactions.json")) {
    "https://raw.githubusercontent.com/evelinag/StarWars-social-network/refs/heads/master/networks/starwars-full-interactions.json" |>
      download.file("data/starwars-full-interactions.json")
  }

  "data/starwars-full-interactions.json" |>
    flatten_network() |>
    write_csv("data/interactions.csv")
}

if (!file.exists("data/miserables.csv")) {
  if (!file.exists("data/miserables.json")) {
    "https://raw.githubusercontent.com/mbostock/vega/066309624c45b1ab15e0abbc295f90878b2f33a7/docs/data/miserables.json" |>
    # "https://bost.ocks.org/mike/miserables/miserables.json" |>
      download.file("data/miserables.json")
  }

  "data/miserables.json" |>
    flatten_network() |>
    write_csv("data/miserables.csv")
}
