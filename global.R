# Load and install packages

pacman::p_load(tidyverse, janitor, data.table, here, rlang, shinydashboard, shiny, DT, bslib, plotly, shinyWidgets, rsconnect)

install.packages("http://cran.r-project.org/src/contrib/Archive/curl/curl_6.2.3.tar.gz", repos=NULL, type="source")
# install.packages("rsconnect", type = "binary")
# library(rsconnect)

# Set directory for data files
# data_path <- "/Users/matthewadams/Documents/R Projects/Baseball Data Analysis/Data/LSU"

data_path <- here("Data")

# List data files to compile
data_list <- list.files(path = data_path, pattern = "sportsref_download", recursive = TRUE, full.names = TRUE)

# Function to read and clean a file
process_file <- function(file) {
  read_csv(file, show_col_types = FALSE) %>% 
    clean_names()
}

# Create standard file format 
template_df <- read_csv(here("Data", "LSU", "sportsref_download_LSU_2025.csv")) %>% 
  clean_names()

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
  
  # Extract school and year from filename
  file_base <- basename(file)
  
  school <- str_extract(file_base, "(?<=sportsref_download_).+?(?=_[0-9]{4})")
  year   <- str_extract(file_base, "\\d{4}")
  
  df <- df %>%
    mutate(
      school = school,
      year = as.integer(year)
    )
  
  return(df)
}

# Combine all data files
all_data <- map_dfr(data_list, ~ suppressWarnings(read_and_clean(.x, col_types_template)))

# Final Data
df <- all_data %>% 
  mutate(name = str_replace_all(name, "[^A-Za-z ]", "")) %>% 
  filter(!is.na(rk)) %>% 
  select(-notes) %>% 
  relocate(year, .before = everything()) %>% 
  relocate(school, .after = year)

# Identify user slider input ranges
min_ip <- floor(min(df$ip, na.rm = TRUE))
max_ip <- ceiling(max(df$ip, na.rm = TRUE))

# Identify seasons
schools <- sort(unique(as.character(df$school)))
seasons <- sort(unique(as.character(df$year)))




