library(tidyverse)
library(DBI)
library(RSQLite)
library(igraph)

merged_res <- tibble(
  region = integer(),
  year = integer(),
  month = integer(),
  in_strength = double(),
  out_strength = double(),
  total_strength = double(),
  pg_index = double(),
  .rows = 0
)

for (year in 2023:2024) {
  for (month in 1:12) {
    if (year == 2024 & month == 12) {
      break
    }
    
    db_file <- paste0("./database/baidu_migration_",
                      year,
                      sprintf("%02d", month),
                      ".sqlite")
    conn <- dbConnect(RSQLite::SQLite(), db_file)
    
    cities <- tbl(conn, "region") %>%
      filter(dt == "city",
             region != 659010)
    
    cityrank <- tbl(conn, "cityrank") %>%
      rename(percent = value)
    
    historycurve <- tbl(conn, "historycurve")
    
    full_cityrank <- cities %>%
      inner_join(historycurve, by = "region") %>%
      inner_join(cityrank, 
                 by = c(region = "source", date = "date", move_in = "move_in")) %>%
      filter(move_in == 1) %>%
      mutate(weight = value * percent / 100) %>%
      select(-dt, -move_in, -value, -percent) %>%
      group_by(region, target) %>%
      summarise(weight = sum(weight, na.rm = TRUE))
    
    graph_cityrank <- graph_from_data_frame(full_cityrank)
    weight <- E(graph_cityrank)$weight
    
    rev_graph <- reverse_edges(graph_cityrank)
    rev_weight <- E(rev_graph)$weight
      
    in_strength <- strength(graph_cityrank, mode = "in", weights = weight)
    out_strength <- strength(graph_cityrank, mode = "out", weights = weight)
    total_strength <- strength(graph_cityrank, mode = "total", weights = weight)
    pg_index <- page_rank(rev_graph, weights = rev_weight)
    
    merged_res <- tibble(
      region = as.integer(names(in_strength)),
      year = year,
      month = month,
      in_strength = in_strength,
      out_strength = out_strength,
      total_strength = total_strength,
      pg_index = pg_index[["vector"]]
    ) %>%
      add_row(merged_res, .)
    
    dbDisconnect(conn)
  }
}

merged_res <- merged_res %>%
  filter(region != 659010)

write_rds(merged_res, "baidu_migration_topological_features.rds")
