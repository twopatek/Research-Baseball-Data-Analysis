# Load and install packages
pacman::p_load(tidyverse, baseballr, janitor, data.table, here, rlang, shinydashboard, shiny, DT, bslib, plotly, shinyWidgets)

# Set directory for data files
data_path <- here("Baseball Data Analysis")

# List data files to compile
data_list <- list.files(pattern = "sportsref_download")

# Function to read and clean a file
process_file <- function(file) {
  read_csv(file, show_col_types = FALSE) %>% 
    clean_names()
}

# Create standard file format 
template_df <- read_csv("sportsref_download_LSU_2025.csv") %>% clean_names()
col_types_template <- map_chr(template_df, ~ class(.x)[1])

# Function to read and clean data files
read_and_clean <- function(file, col_types) {
  df <- read_csv(file, show_col_types = FALSE) %>% clean_names()
  
  for (col in names(col_types)) {  # Loop over every column name in the type template
    if (col %in% names(df)) {      # Only run if that column exists in the current file’s dataframe
      df[[col]] <- switch(         # Coerce the column to the desired type using the template
        col_types[[col]],          # This fetches the desired type for this column, e.g. "character"
        
        character = as.character(df[[col]]),  # If type is "character", convert it
        numeric   = as.numeric(df[[col]]),    # If type is "numeric", convert it
        integer   = as.integer(df[[col]]),    # If type is "integer", convert it
        logical   = as.logical(df[[col]]),    # If type is "logical", convert it
        
        df[[col]]  # default: if type doesn't match any above, leave it unchanged
      )
    }
  }
  
  # Extract year from file name and add it
  year <- str_extract(file, "\\d{4}")
  df <- mutate(df, year = as.integer(year))
  
  return(df)
}

# Combine all data files
all_data <- map_dfr(data_list, ~ suppressWarnings(read_and_clean(.x, col_types_template)))

# Final Data
df <- all_data %>% 
  mutate(name = str_replace_all(name, "[^A-Za-z ]", "")) %>% 
  filter(!is.na(rk)) %>% 
  relocate(year, .before = everything()) %>% 
  select(year, name, era, ip, bb, bb9, so, so9, h, h9, hr, hr9, whip)

# Identify user slider input ranges
min_ip <- floor(min(df$ip, na.rm = TRUE))
max_ip <- ceiling(max(df$ip, na.rm = TRUE))

# Identify seasons
seasons <- unique(df$year)







