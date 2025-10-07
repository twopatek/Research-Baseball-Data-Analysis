# Load and packages
library(dplyr)
library(shiny)
library(shinydashboard)
library(magrittr)
library(DT)
library(plotly)
library(shinyWidgets)

# List data files to compile
data_list <- list.files(
  path = here::here("Data"),
  pattern = "sportsref_download",
  recursive = TRUE,
  full.names = TRUE
)

# Create standard file format
template_df <- readr::read_csv(here::here(
  "Data",
  "LSU",
  "sportsref_download_LSU_2025.csv"
)) |>
  janitor::clean_names()

col_types_template <- map_chr(template_df, ~ class(.x)[1])

# Function to read and clean data files
read_and_clean <- function(file, col_types) {
  df <- readr::read_csv(file, show_col_types = FALSE) |> janitor::clean_names()

  for (col in names(col_types)) {
    # Loop over every column name in the type template
    if (col %in% names(df)) {
      # Only run if that column exists in the current file’s dataframe
      df[[col]] <- switch(
        # Coerce the column to the desired type using the template
        col_types[[col]], # This fetches the desired type for this column, e.g. "character"

        character = as.character(df[[col]]), # If type is "character", convert it
        numeric = as.numeric(df[[col]]), # If type is "numeric", convert it
        integer = as.integer(df[[col]]), # If type is "integer", convert it
        logical = as.logical(df[[col]]), # If type is "logical", convert it

        df[[col]] # default: if type doesn't match any above, leave it unchanged
      )
    }
  }

  # Extract school and year from filename
  file_base <- basename(file)
  school <- stringr::str_extract(
    file_base,
    "(?<=sportsref_download_).*?(?=_[0-9]{4})"
  )
  year <- stringr::str_extract(file_base, "\\d{4}")

  df <- df |>
    mutate(
      school = school,
      year = as.integer(year)
    )

  return(df)
}

# Combine all data files
all_data <- map_dfr(data_list, ~ read_and_clean(.x, col_types_template))

# Final Data
df <- all_data |>
  mutate(name = stringr::str_replace_all(name, "[^A-Za-z ]", "")) |>
  filter(!is.na(rk)) |>
  filter(year > 2011) |>
  select(-notes) |>
  relocate(year, .before = everything()) |>
  relocate(school, .after = year)

# Identify user slider input ranges
min_ip <- floor(min(df$ip, na.rm = TRUE))
max_ip <- ceiling(max(df$ip, na.rm = TRUE))

# Identify seasons
schools <- sort(unique(as.character(df$school)))
seasons <- sort(unique(as.character(df$year)))

# Define stats to rate and their direction
rating_stats <- tribble(
  ~stat,
  ~higher_is_better,
  ~prior_weight,
  "era",
  FALSE,
  40,
  "fip",
  FALSE,
  30,
  "so9",
  TRUE,
  15,
  "bb9",
  FALSE,
  20,
  "hr9",
  FALSE,
  25,
  "whip",
  FALSE,
  25,
  "so_w",
  TRUE,
  10,
  "k_pct",
  TRUE,
  15,
  "bb_pct",
  FALSE,
  15
)

# Define stat weights for composite
stat_weights <- c(
  era_rating = 0.20,
  fip_rating = 0.20,
  so9_rating = 0.15,
  bb9_rating = 0.10,
  hr9_rating = 0.10,
  whip_rating = 0.10,
  so_w_rating = 0.05,
  k_pct_rating = 0.05,
  bb_pct_rating = 0.05
)

rating_stats <- rating_stats |> arrange(stat)
stat_weights <- stat_weights[order(names(stat_weights))]

player_ratings_df <- df |>
  mutate(
    fip = case_when(
      is.na(hr) | is.na(bb) | is.na(so) | is.na(ip) ~ NA_real_,
      ip < 10 ~ NA_real_, # Replace 10 with your desired fip_min_ip
      TRUE ~ round(((13 * hr) + (3 * bb) - (2 * so)) / ip + 3.1, 3)
    ),
    k_pct = case_when(
      is.na(so) | is.na(bf) | bf < 10 ~ NA_real_,
      TRUE ~ round(so / bf, 3)
    ),
    bb_pct = case_when(
      is.na(bb) | is.na(bf) | bf < 10 ~ NA_real_,
      TRUE ~ round(bb / bf, 3)
    )
  )

generate_player_ratings <- function(
  player_ratings_df,
  rating_stats,
  stat_weights
) {
  req(player_ratings_df)
  validate(need(
    nrow(player_ratings_df) > 0,
    "No data available for selected filters."
  ))

  # Compute league averages per year
  league_avgs <- player_ratings_df |>
    filter(ip > 0) |>
    group_by(year) |>
    summarize(
      across(
        all_of(rating_stats$stat),
        ~ round(mean(.x, na.rm = TRUE), 3),
        .names = "avg_{.col}"
      ),
      .groups = "drop"
    )

  df_joined <- player_ratings_df |>
    left_join(league_avgs, by = "year")

  # Create named vector for priors
  prior_weights <- rating_stats$prior_weight
  names(prior_weights) <- rating_stats$stat

  # Shrinkage
  shrunk_stats <- map2_dfc(
    rating_stats$stat,
    prior_weights,
    ~ {
      stat <- .x
      prior <- .y
      avg_col <- paste0("avg_", stat)
      shrunk_col <- paste0("shrunk_", stat)

      tibble(
        !!shrunk_col := case_when(
          df_joined$ip > 0 &
            is.finite(df_joined[[stat]]) &
            is.finite(df_joined[[avg_col]]) ~
            round(
              (df_joined$ip *
                df_joined[[stat]] +
                prior * df_joined[[avg_col]]) /
                (df_joined$ip + prior),
              3
            ),
          TRUE ~ NA_real_
        )
      )
    }
  )

  df_joined <- bind_cols(df_joined, shrunk_stats)

  # Add rating column metadata
  rating_stats <- rating_stats |>
    mutate(
      shrunk_col = paste0("shrunk_", stat),
      rating_col = paste0(stat, "_rating"),
      invert = !higher_is_better
    )

  # Rescale ratings
  rescale_ratings_by_year <- function(df_group) {
    rating_cols <- pmap_dfc(
      rating_stats,
      function(
        stat,
        higher_is_better,
        prior_weight,
        shrunk_col,
        rating_col,
        invert
      ) {
        values <- df_group[[shrunk_col]]
        values <- if (invert) -values else values
        tibble(
          !!rating_col := round(
            scales::rescale(values, to = c(0, 100), na.rm = TRUE),
            0
          )
        )
      }
    )
    bind_cols(df_group, rating_cols)
  }

  df_ratings <- df_joined |>
    group_by(year) |>
    group_split() |>
    map_dfr(rescale_ratings_by_year) |>
    ungroup()

  # Composite score
  valid_weights <- stat_weights[names(stat_weights) %in% colnames(df_ratings)]
  rating_names <- names(valid_weights)

  df_ratings <- df_ratings %>%
    mutate(
      composite_rating = round(
        rowSums(
          select(., all_of(rating_names)) * rep(valid_weights, each = nrow(.)),
          na.rm = TRUE
        ),
        0
      )
    )

  # Output tables
  core_cols <- c("year", "school", "name", "ip", "composite_rating")
  stat_column_blocks <- lapply(rating_stats$stat, function(stat) {
    c(
      stat,
      paste0("avg_", stat),
      paste0("shrunk_", stat),
      paste0(stat, "_rating")
    )
  })

  final_detailed_cols <- c(core_cols, unlist(stat_column_blocks))

  detailed <- df_ratings |>
    select(all_of(final_detailed_cols)) |>
    arrange(desc(composite_rating))

  summary <- df_ratings |>
    select(all_of(c(core_cols, rating_names))) |>
    arrange(desc(composite_rating))

  return(list(summary = summary, detailed = detailed))
}

# Precomputed once
all_ratings <- generate_player_ratings(
  player_ratings_df,
  rating_stats,
  stat_weights
)
