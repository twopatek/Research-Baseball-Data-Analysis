# Define server logic
server <- function(input, output, session) {
  ### Leaderboard Menu Item ###

  # Render slider input
  output$ip_slider_ui <- renderUI({
    req(df)

    min_ip <- 1
    max_ip <- ceiling(max(df$ip, na.rm = TRUE))

    sliderInput(
      "ip_range",
      "Select Innings Pitched Range:",
      min = min_ip,
      max = max_ip,
      value = c(30, max_ip),
      step = 1
    )
  })

  # Top Strikeouts by Team-Year
  output$top_so_teams <- renderDT({
    df %>%
      mutate(year_team = paste(year, school)) %>%
      group_by(year_team) %>%
      summarize(so = round(sum(so, na.rm = TRUE), 0)) %>%
      arrange(desc(so)) %>%
      datatable(
        options = list(
          paging = FALSE,
          searching = FALSE,
          info = FALSE,
          ordering = TRUE,
          scrollY = "200px",
          dom = 't'
        ),
        rownames = FALSE
      )
  })

  # Top Strikeouts by Player-Year
  output$top_so_players <- renderDT({
    req(input$ip_range)

    df %>%
      filter(ip >= input$ip_range[1], ip <= input$ip_range[2]) %>%
      mutate(year_school_player = paste(year, school, name)) %>%
      select(year_school_player, ip, so) %>%
      arrange(desc(so)) %>%
      datatable(
        options = list(
          paging = FALSE,
          searching = FALSE,
          info = FALSE,
          ordering = TRUE,
          scrollY = "200px",
          dom = 't'
        ),
        rownames = FALSE
      )
  })

  # Top ERA by Team-Year
  output$top_era_teams <- renderDT({
    req(input$ip_range)

    df %>%
      mutate(year_team = paste(year, school)) %>%
      group_by(year_team) %>%
      summarize(era = round(mean(era, na.rm = TRUE), 3)) %>%
      arrange(era) %>%
      datatable(
        options = list(
          paging = FALSE,
          searching = FALSE,
          info = FALSE,
          ordering = TRUE,
          scrollY = "200px",
          dom = 't'
        ),
        rownames = FALSE
      )
  })

  # Top ERA by Player-Year
  output$top_era_players <- renderDT({
    req(input$ip_range)

    df %>%
      filter(ip >= input$ip_range[1], ip <= input$ip_range[2]) %>%
      mutate(year_school_player = paste(year, school, name)) %>%
      select(year_school_player, ip, era) %>%
      arrange(era) %>%
      datatable(
        options = list(
          paging = FALSE,
          searching = FALSE,
          info = FALSE,
          ordering = TRUE,
          scrollY = "200px",
          dom = 't'
        ),
        rownames = FALSE
      )
  })

  # Top WHIP by Team-Year
  output$top_whip_teams <- renderDT({
    req(input$ip_range)

    df %>%
      mutate(year_team = paste(year, school)) %>%
      group_by(year_team) %>%
      summarize(whip = round(mean(whip, na.rm = TRUE), 3)) %>%
      arrange(whip) %>%
      datatable(
        options = list(
          paging = FALSE,
          searching = FALSE,
          info = FALSE,
          ordering = TRUE,
          scrollY = "200px",
          dom = 't'
        ),
        rownames = FALSE
      )
  })

  # Top WHIP by Player-Year
  output$top_whip_players <- renderDT({
    req(input$ip_range)

    df %>%
      filter(ip >= input$ip_range[1], ip <= input$ip_range[2]) %>%
      mutate(year_school_player = paste(year, school, name)) %>%
      select(year_school_player, ip, whip) %>%
      arrange(whip) %>%
      datatable(
        options = list(
          paging = FALSE,
          searching = FALSE,
          info = FALSE,
          ordering = TRUE,
          scrollY = "200px",
          dom = 't'
        ),
        rownames = FALSE
      )
  })

  ### Player Rating Tab ###

  # Render both ui elements
  output$shared_rating_inputs <- renderUI({
    tagList(
      pickerInput(
        inputId = "rating_schools",
        label = "Select School(s)",
        choices = schools,
        selected = schools,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE,
          `selected-text-format` = "count > 3"
        )
      ),
      pickerInput(
        inputId = "rating_years",
        label = "Select Year(s)",
        choices = seasons,
        selected = seasons,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE,
          `selected-text-format` = "count > 3"
        )
      )
    )
  })

  # Reactive values for ratings data
  ratings_data <- reactiveVal()
  detailed_data <- reactiveVal()

  ratings_data(all_ratings$summary)
  detailed_data(all_ratings$detailed)

  # Reactive ratings
  filtered_ratings <- reactive({
    req(ratings_data(), input$rating_schools, input$rating_years)

    ratings_data() %>%
      filter(year %in% input$rating_years, school %in% input$rating_schools)
  })

  filtered_detailed <- reactive({
    req(detailed_data(), input$rating_schools, input$rating_years)

    detailed_data() %>%
      filter(year %in% input$rating_years, school %in% input$rating_schools)
  })

  # Ratings Table
  output$ratings_table <- renderDT(
    {
      filtered_ratings()
    },
    options = list(
      pageLength = -1,
      scrollY = "500px",
      scrollX = TRUE,
      paging = FALSE
    )
  )

  # Methodology Table
  output$methodology_table <- renderDT(
    {
      filtered_detailed()
    },
    options = list(
      pageLength = -1,
      scrollY = "500px",
      scrollX = TRUE,
      paging = FALSE
    )
  )

  # Recalculate logic placeholder
  observeEvent(input$update_ratings, {
    withProgress(
      message = "Calculating player ratings. Please wait...",
      value = 0.5,
      {
        raw_weights <- sapply(rating_stats$stat, function(stat) {
          input[[paste0("weight_", stat)]]
        })
        names(raw_weights) <- paste0(rating_stats$stat, "_rating")

        weight_sum <- sum(raw_weights, na.rm = TRUE)
        if (weight_sum == 0) {
          normalized_weights <- rep(
            1 / length(raw_weights),
            length(raw_weights)
          ) # fallback: equal weights
        } else {
          normalized_weights <- raw_weights / weight_sum
        }

        updated_priors <- sapply(rating_stats$stat, function(stat) {
          input[[paste0("prior_", stat)]]
        })

        updated_rating_stats <- rating_stats %>%
          mutate(prior_weight = updated_priors)

        rating_input_df <- player_ratings_df %>%
          filter(ip > 0)

        # Run ratings generation
        new_ratings <- generate_player_ratings(
          rating_input_df,
          updated_rating_stats,
          normalized_weights
        )

        ratings_data(new_ratings$summary)
        detailed_data(new_ratings$detailed)
      }
    )
  })

  # Reset methodology stats
  observeEvent(input$reset_rating_inputs, {
    for (stat in rating_stats$stat) {
      updateNumericInput(
        session,
        paste0("weight_", stat),
        value = stat_weights[[paste0(stat, "_rating")]]
      )

      updateNumericInput(
        session,
        paste0("prior_", stat),
        value = rating_stats$prior_weight[rating_stats$stat == stat]
      )
    }
  })

  # Render warning output for weights
  output$weight_sum_warning <- renderText({
    raw_weights <- sapply(rating_stats$stat, function(stat) {
      input[[paste0("weight_", stat)]]
    })
    total <- round(sum(raw_weights, na.rm = TRUE), 3)

    if (abs(total - 1) > 0.01) {
      paste("⚠️ Weights sum to", total, "- they will be normalized to 1.0.")
    } else {
      paste("✓ Weights sum to", total)
    }
  })
}
