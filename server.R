# Define server logic 
server <- function(input, output, session) {
  
  ### Leaderboard Menu Item ###
  
  output$ip_slider_ui <- renderUI({
    req(df)
    
    min_ip <- 1
    max_ip <- ceiling(max(df$ip, na.rm = TRUE))
    
    sliderInput("ip_range",
                "Select Innings Pitched Range:",
                min = min_ip,
                max = max_ip,
                value = c(30, max_ip),
                step = 1)
  })
  
  
  # Top 5 Strikeouts by Team-Year
  output$top_so_teams <- renderDT({
    
    # req(input$ip_range)
    
    df %>%
      # filter(ip >= input$ip_range[1], ip <= input$ip_range[2]) %>% 
      mutate(year_team = paste(year, school)) %>%
      group_by(year_team) %>%
      summarize(so = round(sum(so, na.rm = TRUE), 0)) %>%
      arrange(desc(so)) %>%
      slice_head(n = 5) %>%
      datatable(options = list(dom = 't'), rownames = FALSE)
  })
  
  # Top 5 Strikeouts by Player-Year
  output$top_so_players <- renderDT({
    
    req(input$ip_range)
    
    df %>%
      filter(ip >= input$ip_range[1], ip <= input$ip_range[2]) %>% 
      mutate(school_year_player = paste(year, school, name)) %>%
      select(school_year_player, ip, so) %>% 
      arrange(desc(so)) %>%
      slice_head(n = 5) %>%
      datatable(options = list(dom = 't'), rownames = FALSE)
  })
  
  # Top 5 ERA by Team-Year
  output$top_era_teams <- renderDT({
    
    req(input$ip_range)
    
    df %>%
      # filter(ip >= input$ip_range[1], ip <= input$ip_range[2]) %>% 
      mutate(year_team = paste(year, school)) %>%
      group_by(year_team) %>%
      summarize(era = round(mean(era, na.rm = TRUE), 3)) %>%
      arrange(era) %>%
      slice_head(n = 5) %>%
      datatable(options = list(dom = 't'), rownames = FALSE)
  })
  
  # Top 5 ERA by Player-Year
  output$top_era_players <- renderDT({
    
    req(input$ip_range)
    
    df %>%
      filter(ip >= input$ip_range[1], ip <= input$ip_range[2]) %>% 
      mutate(school_year_player = paste(year, school, name)) %>%
      select(school_year_player, ip, era) %>% 
      arrange(era) %>%
      slice_head(n = 5) %>%
      datatable(options = list(dom = 't'), rownames = FALSE)
  })
  
  # Top 5 WHIP by Team-Year
  output$top_whip_teams <- renderDT({
    
    req(input$ip_range)
    
    df %>%
      # filter(ip >= input$ip_range[1], ip <= input$ip_range[2]) %>% 
      mutate(year_team = paste(year, school)) %>%
      group_by(year_team) %>%
      summarize(whip = round(mean(whip, na.rm = TRUE), 3)) %>%
      arrange(whip) %>%
      slice_head(n = 5) %>%
      datatable(options = list(dom = 't'), rownames = FALSE)
  })
  
  # Top 5 WHIP by Player-Year
  output$top_whip_players <- renderDT({
    
    req(input$ip_range)
    
    df %>%
      filter(ip >= input$ip_range[1], ip <= input$ip_range[2]) %>% 
      mutate(school_year_player = paste(year, school, name)) %>%
      select(school_year_player, ip, whip) %>% 
      arrange(whip) %>%
      slice_head(n = 5) %>%
      datatable(options = list(dom = 't'), rownames = FALSE)
  })
  
  ### Data Menu Item ###
  
  # Render dynamic school selector
  observe({
    if (input$data_school_select_all) {
      updateSelectInput(session, "data_schools", selected = schools)
    } else {
      updateSelectInput(session, "data_schools", selected = first(schools))
    }
  })
  
  # Render dynamic year selector
  observe({
    if (input$year_select_all) {
      updateSelectInput(session, "years", selected = seasons)
    } else {
      updateSelectInput(session, "years", selected = max(seasons))
    }
  })
  
  # Render dynamic slider to filter innings pitched
  output$ip_slider <- renderUI({
    req(df)
    
    min_ip <- floor(min(df$ip, na.rm = TRUE))
    max_ip <- ceiling(max(df$ip, na.rm = TRUE))
    
    sliderInput(
      "var",
      label = "Choose the minimum or maximum innings pitched",
      min = min_ip,
      max = max_ip,
      value = c(1, max_ip)
    )
  })
  
  # Button to reset raw data table
  observeEvent(input$reset_data_table, {
    updateSelectInput(session, "data_schools", selected = first(schools))
    updateCheckboxInput(session, "data_school_select_all", value = FALSE)
    
    updateSelectInput(session, "years", selected = "2025")
    updateCheckboxInput(session, "year_select_all", value = FALSE)
    
    updateSliderInput(session, "var", value = c(1, max(df$ip, na.rm = TRUE)))
  })
  

  # Reactive raw data
  filtered_raw_df <- reactive({
    req(input$data_schools, input$years, input$var)
    
    df %>%
      filter(
        school %in% input$data_schools,
        year %in% input$years,
        ip >= input$var[1],
        ip <= input$var[2]
      )
  })
  
  # Render table showing raw data loaded for analysis
  output$raw_data <- renderDT({
    datatable(
      filtered_raw_df(),
      class = 'cell-border stripe',
      options = list(
        scrollY = TRUE, 
        scrollX = "100px",
        scrollCollapse = TRUE,
        fixedHeader = TRUE,
        pageLength = 10)
    )
  })
  
  
  ### Analysis Menu Item ###
  
  ## Report Tab Panel ##
  
  # Reactive report data
  filtered_report_df <- reactive({
    req(input$report_schools)
    
    df %>%
      select(year, school, name, era, ip, bb, bb9, so, so9, h, h9, hr, hr9, whip) %>% 
      filter(school %in% input$report_schools)
  })
  
  observeEvent(input$report_school_select_all, {
    if (input$report_school_select_all) {
      updateSelectInput(session, "report_schools", selected = schools)
    } else {
      updateSelectInput(session, "report_schools", selected = character(0))
    }
  }, ignoreInit = TRUE)
  
  
  # Render UI
  output$report_var_selector <- renderUI({
    req(filtered_report_df())
    
    numeric_vars <- filtered_report_df() %>%
      select(where(is.numeric)) %>%
      select(-year) %>%
      names()
    
    checkboxGroupInput(
      inputId = "selected_vars",
      label = "Choose variables to summarize:",
      choices = numeric_vars,
      selected = numeric_vars[1]  # preselect first few
    )
  })
  
  # Reactive summary triggered by action button
  summary_data <- eventReactive(input$summary_stats, {
    # Ensure inputs exist
    req(input$selected_vars)
    
    filtered_report_df() %>%
      filter(ip > 0) %>%
      group_by(year, school) %>%
      summarise(across(
        all_of(input$selected_vars),
        list(
          mean = ~round(mean(.x, na.rm = TRUE), 3),
          median = ~round(median(.x, na.rm = TRUE), 3),
          sd = ~round(sd(.x, na.rm = TRUE), 3)
        ),
        .names = "{.col}_{.fn}"
      ))
  })
  
  # Store original data as default
  report_mode <- reactiveVal("full")
  
  # Update mode when button is clicked
  observeEvent(input$summary_stats, {
    report_mode("summary")
  })
  
  # Reset table data
  observeEvent(input$reset_report_table, {
    report_mode("full")
  })
  
  # Create dynamic table output
  output$analysis_report <- renderDT({
    if (report_mode() == "full") {
      # Initial full data (excluding name)
      filtered_report_df() %>%
        filter(ip > 0) %>%
        datatable(
          class = 'cell-border stripe',
          options = list(
            scrollX = TRUE,
            scrollY = TRUE,
            pageLength = 10)
        )
    } else {
      # Show summary data
      req(summary_data())
      datatable(
        summary_data(),
        class = 'cell-border stripe',
        options = list(
          scrollX = TRUE, 
          scrollY = TRUE,
          pageLength = 10)
      )
    }
  })
  
  ### Advanced Statistics Panel Server Logic ###
  
  # Observe school select/deselect all toggle
  observeEvent(input$adv_school_select_all, {
    if (input$adv_school_select_all) {
      updateSelectInput(session, "adv_schools", selected = schools)
    } else {
      updateSelectInput(session, "adv_schools", selected = character(0))
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$adv_year_select_all, {
    if (input$adv_year_select_all) {
      updateSelectInput(session, "adv_years", selected = seasons)
    } else {
      updateSelectInput(session, "adv_years", selected = character(0))
    }
  }, ignoreInit = TRUE)
  
  
  # Reactive expression to filter dataset for advanced stats
  filtered_adv_df <- eventReactive(input$calc_adv_stats, {
    req(input$adv_schools, input$adv_years)
    
    df %>%
      filter(
        school %in% input$adv_schools,
        year %in% input$adv_years
      ) %>%
      mutate(
        fip = case_when(
          is.na(hr) | is.na(bb) | is.na(so) | is.na(ip) ~ NA_real_,
          ip < input$fip_min_ip ~ NA_real_,
          TRUE ~ round(((input$hr_weight * hr) + (input$bb_weight * bb) - (input$so_weight * so)) / ip + input$fip_constant, 3)
        ),
        k_pct = case_when(
          is.na(so) | is.na(bf) | bf < input$k_pct_min_bf ~ NA_real_,
          TRUE ~ round(so / bf, 3)
        ),
        bb_pct = case_when(
          is.na(bb) | is.na(bf) | bf < input$bb_pct_min_bf ~ NA_real_,
          TRUE ~ round(bb / bf, 3)
        ),
        k_bb = case_when(
          is.na(so) | is.na(bb) | is.na(ip) ~ NA_real_,
          ip < input$k_bb_min_ip ~ NA_real_,
          bb < input$k_bb_min_bb ~ NA_real_,
          bb == 0 ~ NA_real_,
          TRUE ~ round(so / bb, 3)
        ),
        babip_denominator = bf - so - bb - hbp - hr,
        babip = case_when(
          is.na(h) | is.na(hr) | is.na(so) | is.na(bb) | is.na(hbp) | is.na(bf) | is.na(ip) ~ NA_real_,
          ip < input$babip_min_ip ~ NA_real_,
          babip_denominator <= 0 ~ NA_real_,
          TRUE ~ round((h - hr) / babip_denominator, 3)
        )
      ) %>% 
      select(year, school, name, w, l, w_l_percent, ip, bf, fip, k_pct, bb_pct, k_bb, babip)
  })
  
  # Reset advanced stats input controls
  observeEvent(input$reset_adv_stats, {
    updateSelectInput(session, "adv_schools", selected = first(schools))
    updateCheckboxInput(session, "adv_school_select_all", value = TRUE)
    
    updateSelectInput(session, "adv_years", selected = "2025")
    updateCheckboxInput(session, "adv_year_select_all", value = TRUE)
    
    updateNumericInput(session, "hr_weight", value = 13)
    updateNumericInput(session, "bb_weight", value = 3)
    updateNumericInput(session, "so_weight", value = 2)
    updateNumericInput(session, "fip_constant", value = 3.1)
    updateNumericInput(session, "fip_min_ip", value = 10)
    
    updateNumericInput(session, "k_pct_min_bf", value = 10)
    updateNumericInput(session, "bb_pct_min_bf", value = 10)
    updateNumericInput(session, "k_bb_min_bb", value = 1)
    updateNumericInput(session, "k_bb_min_ip", value = 10)
    updateNumericInput(session, "babip_min_ip", value = 10)
  })
  
  # Render advanced stats output
  output$adv_stats_output <- renderDT({
    req(filtered_adv_df())
    
    datatable(
      filtered_adv_df(),
      class = 'cell-border stripe',
      options = list(
        scrollX = TRUE,
        scrollY = TRUE,
        pageLength = 10
      )
    )
  })
  
  ## Plot Tab Panel ##
  
  # Reactive plot data
  filtered_plot_df <- reactive({
    req(input$plot_schools)
    
    df %>%
      select(year, school, name, era, ip, bb, bb9, so, so9, h, h9, hr, hr9, whip) %>% 
      filter(school %in% input$plot_schools)
  })
  
  # Render UI
  output$plot_var_selector <- renderUI({
    req(filtered_plot_df())
    numeric_vars <- filtered_plot_df() %>%
      select(where(is.numeric)) %>%
      select(-year) %>%
      names()
    
    tagList(
      selectInput("plot_var", "Select variable to plot:", choices = numeric_vars),
      radioButtons("plot_mode", "Plot Mode:",
                   choices = c("Raw Only" = "raw", 
                               "Summary Only" = "summary", 
                               "Both" = "both"),
                   selected = "raw")
    )
  })
  
  # Render output
  output$analysis_plot <- renderPlotly({
    # Ensure inputs exist
    req(input$plot_var, input$plot_mode, input$plot_schools)
    
    # Capture the selected variable as a symbol
    var_sym <- sym(input$plot_var)
    
    # Raw data: mean per year
    raw_data <- filtered_plot_df() %>%
      filter(ip > 0) %>%
      group_by(year, school) %>%
      summarise(value = mean(!!var_sym, na.rm = TRUE), .groups = "drop") %>%
      mutate(type = "Raw")
    
    # Summary statistics (mean, median, sd)
    summary_data <- filtered_plot_df() %>%
      filter(ip > 0) %>%
      group_by(year, school) %>%
      summarise(
        mean   = mean(!!var_sym, na.rm = TRUE),
        median = median(!!var_sym, na.rm = TRUE),
        sd     = sd(!!var_sym, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Reshape summary stats to long format for easier plotting
    summary_long <- summary_data %>%
      pivot_longer(cols = c(mean, median, sd), names_to = "type", values_to = "value")
    
    # Decide what to plot
    plot_data <- switch(input$plot_mode,
                        raw = raw_data,
                        summary = summary_long,
                        both = bind_rows(raw_data, summary_long))
    
    # Plotly bar chart
    plot_ly(plot_data,
            x = ~year,
            y = ~value,
            color = ~type,
            type = "bar") %>%
      layout(barmode = "group",
             title = paste("Yearly", input$plot_var, "Analysis"),
             yaxis = list(title = input$plot_var),
             xaxis = list(title = "Year"))
  })
  
}


