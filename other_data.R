

if (!file.exists("data/interactions.csv")) {
  if (!file.exists("data/starwars-full-interactions.json")) {
    "https://raw.githubusercontent.com/evelinag/StarWars-social-network/refs/heads/master/networks/starwars-full-interactions.json" |>
      download.file("data/starwars-full-interactions.json")
  }

  "data/starwars-full-interactions.json" |>
    network_json2df() |>
    write_csv("data/interactions.csv")
}

if (!file.exists("data/miserables.csv")) {
  if (!file.exists("data/miserables.json")) {
    "https://raw.githubusercontent.com/mbostock/vega/066309624c45b1ab15e0abbc295f90878b2f33a7/docs/data/miserables.json" |>
    # "https://bost.ocks.org/mike/miserables/miserables.json" |>
      download.file("data/miserables.json")
  }

  "data/miserables.json" |>
    network_json2df() |>
    write_csv("data/miserables.csv")
}
