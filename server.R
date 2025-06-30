# Define server logic 
server <- function(input, output, session) {
  
  ### Data Menu Item ###
  
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
  
  
  
  # Render dynamic year selector
  observe({
    if (input$input_select_all) {
      updateSelectInput(session, "years", selected = seasons)
    } else {
      updateSelectInput(session, "years", selected = character(0))
    }
  })

  # Reactive raw data
  filtered_df <- reactive({
    req(input$years, input$var)
    
    df %>%
      filter(
        year %in% input$years,
        ip >= input$var[1],
        ip <= input$var[2]
      )
  })
  
  
  # Render table showing raw data loaded for analysis
  output$raw_data <- renderDT({
    datatable(
      filtered_df(),
      class = 'cell-border stripe',
      options = list(scrollY = TRUE, pageLength = 10)
    )
  })
  
  
  ### Analysis Menu Item ###
  
  ## Report Tab Panel ##
  
  # Render UI
  output$report_var_selector <- renderUI({
    req(df)
    numeric_vars <- df %>%
      select(where(is.numeric)) %>%
      select(-year) %>%
      names()
    
    checkboxGroupInput(
      inputId = "selected_vars",
      label = "Choose variables to summarize:",
      choices = numeric_vars,
      selected = numeric_vars[1:3]  # preselect first few
    )
  })
  
  # Reactive summary triggered by action button
  summary_data <- eventReactive(input$summary_stats, {
    # Ensure inputs exist
    req(input$selected_vars)
    
    df %>%
      filter(ip > 0) %>%
      select(-name) %>% 
      group_by(year) %>%
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
  observeEvent(input$reset_table, {
    report_mode("full")
  })
  
  # Create dynamic table output
  output$analysis_report <- renderDT({
    if (report_mode() == "full") {
      # Initial full data (excluding name)
      df %>%
        filter(ip > 0) %>%
        select(-name) %>%
        datatable(
          class = 'cell-border stripe',
          options = list(scrollX = TRUE, pageLength = 15)
        )
    } else {
      # Show summary data
      req(summary_data())
      datatable(
        summary_data(),
        class = 'cell-border stripe',
        options = list(scrollX = TRUE, pageLength = 15)
      )
    }
  })
  
  ## Plot Tab Panel ##
  
  # Render UI
  output$plot_var_selector <- renderUI({
    req(df)
    numeric_vars <- df %>%
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
    req(input$plot_var, input$plot_mode)
    
    # Capture the selected variable as a symbol
    var_sym <- sym(input$plot_var)
    
    # Raw data: mean per year
    raw_data <- df %>%
      filter(ip > 0) %>%
      group_by(year) %>%
      summarise(value = mean(!!var_sym, na.rm = TRUE), .groups = "drop") %>%
      mutate(type = "Raw")
    
    # Summary statistics (mean, median, sd)
    summary_data <- df %>%
      filter(ip > 0) %>%
      group_by(year) %>%
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


