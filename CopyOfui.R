# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "NCAA Pitching Analysis Dashboard"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem("Info", tabName = "info_tab"),
      menuItem("Leaderboard", tabName = "leaderboard_tab"),
      menuItem("Data", tabName = "data_tab"),
      menuItem("Analysis", tabName = "analysis_tab"),
      menuItem("Player Ratings", tabName = "ratings_tab")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "info_tab",
        fluidRow(
          box(
            title = "Project Overview",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            HTML("
        <p>This NCAA Pitching Dashboard is part of an ongoing effort to build a custom database of college baseball pitching statistics.</p>
        <p><strong>Project Goals:</strong></p>
        <ul>
          <li>Consolidate and explore NCAA pitching data across multiple years.</li>
          <li>Provide a dashboard for quick filtering, analysis, and visualization of team and player performance.</li>
        </ul>
        <p><strong>Data Methodology:</strong></p>
        <ul>
          <li>All data is manually downloaded from <a href='https://www.sports-reference.com/cbb/' target='_blank'>Baseball Reference</a> by team and season.</li>
          <li>Files are standardized and cleaned locally before being compiled into the dashboard.</li>
          <li>This process is ongoing, and additional teams will be added over time.</li>
        </ul>
        <p>Built in <code>R</code> using <code>Shiny</code>, <code>tidyverse</code>, and <code>plotly</code>.</p>
        <p><strong>View full project and documentation on GitHub:</strong> <a href='https://github.com/twopatek/Baseball-Data-Analysis/blob/main/README.md' target='_blank'>github.com/twopatek/Baseball-Data-Analysis</a></p>
      ")
          )
        )
      ),
      
      tabItem(
        tabName = "leaderboard_tab",
        fluidRow(
          box(
            title = "Filter Individual Players by Minimum / Maxiumum Innings Pitched",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            uiOutput("ip_slider_ui")
          )
        ),
        fluidRow(
          box(
            title = "Top Strikeouts (Player-Year)",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            div(
              style = "height: 250px; overflow-y: scroll;",
              DTOutput("top_so_teams")
            )
          ),
          box(
            title = "Top 5 Strikeouts (Player-Year)",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            div(
              style = "height: 250px; overflow-y: scroll;",
              DTOutput("top_so_players")
            )
          )
        ),
        fluidRow(
          box(
            title = "Top 5 ERA (Team-Year)",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            div(
              style = "height: 250px; overflow-y: scroll;",
              DTOutput("top_era_teams")
            )
          ),
          box(
            title = "Top 5 ERA (Player-Year)",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            div(
              style = "height: 250px; overflow-y: scroll;",
              DTOutput("top_era_players")
            )
          )
        ),
        fluidRow(
          box(
            title = "Top 5 WHIP (Team-Year)",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            div(
              style = "height: 250px; overflow-y: scroll;",
              DTOutput("top_whip_teams")
              )
          ),
          box(
            title = "Top 5 WHIP (Player-Year)",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            div(
              style = "height: 250px; overflow-y: scroll;",
              DTOutput("top_whip_players")
            )
          )
        )
      ),
      tabItem(
        tabName = "data_tab",
        fluidRow(
          column(
            width = 6,
            box(
              title = "Filter data by school and year",
              solidHeader = TRUE,
              status = "info",
              width = 12,
              selectizeInput(
                inputId = "data_schools",
                label = "Select School(s)",
                choices = schools,
                selected = first(schools),
                multiple = TRUE,
                options = list(
                  placeholder = 'Search or scroll to choose school(s)',
                  maxOptions = 1000,
                  plugins = list('remove_button'),
                  closeAfterSelect = FALSE
                )
              ),
              checkboxInput("data_school_select_all", "Select/Deselect All Schools", value = FALSE),
              selectizeInput(
                inputId = "years",
                label = "Select Year(s)",
                choices = seasons,
                selected = max(seasons),
                multiple = TRUE,
                options = list(
                  placeholder = 'Search or scroll to choose year(s)',
                  plugins = list('remove_button'),
                  maxOptions = 1000
                )
              ),
              checkboxInput("year_select_all", "Select/Deselect All Years", value = FALSE),
              br(),
              actionButton("reset_data_table", "Reset Table")
            )
          ),
          column(
            width = 6,
            box(
              title = "Choose the minimum or maximum innings pitched",
              solidHeader = TRUE,
              status = "info",
              width = 12,
              uiOutput("ip_slider")
            )
          )
        ),
        fluidRow(
          box(
            title = "Filtered Player Data",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("raw_data")
          )
        )
      ),
      tabItem(
        tabName = "analysis_tab",
        fluidRow(
          tabBox(
            width = 12,
            tabPanel("Report Tab", 
                     fluidRow(
                       column(
                         width = 3,
                         box(
                           title = "Report Data Parameters",
                           solidHeader = TRUE,
                           status = "info",
                           width = 12,
                           selectizeInput(
                             inputId = "report_schools",
                             label = "Select School(s)",
                             choices = schools,
                             selected = first(schools),
                             multiple = TRUE,
                             options = list(
                               placeholder = "Search or scroll to choose school(s)",
                               plugins = list("remove_button"),
                               maxOptions = 1000
                             )
                           ),
                           checkboxInput("report_school_select_all", "Select/Deselect All Schools", value = FALSE),
                           uiOutput("report_var_selector"),
                           actionButton("summary_stats", "Calculate Summary Stats"),
                           br(), br(), 
                           actionButton("reset_report_table", "Reset Table")
                         )
                       ),
                       column(
                         width = 9,
                         box(
                           title = "Report Output",
                           solidHeader = TRUE,
                           status = "primary",
                           width = 12,
                           DTOutput("analysis_report")
                         )
                       )
                     )
            ),
            
            tabPanel("Advanced Statistics", 
                     fluidRow(
                       column(
                         width = 3,
                         box(
                           title = "Advanced Stat Controls",
                           solidHeader = TRUE,
                           status = "info",
                           width = 12,
                           
                           # School + Year inputs (unchanged)
                           selectizeInput(
                             inputId = "adv_schools",
                             label = "Select School(s)",
                             choices = schools,
                             selected = first(schools),
                             multiple = TRUE,
                             options = list(
                               placeholder = "Search or scroll to choose school(s)",
                               plugins = list("remove_button"),
                               maxOptions = 1000
                             )
                           ),
                           checkboxInput("adv_school_select_all", "Select/Deselect All Schools", value = FALSE),
                           
                           selectizeInput(
                             inputId = "adv_years",
                             label = "Select Year(s)",
                             choices = seasons,
                             selected = max(seasons),
                             multiple = TRUE,
                             options = list(
                               placeholder = "Search or scroll to choose school(s)",
                               plugins = list("remove_button"),
                               maxOptions = 1000
                             )
                           ),
                           checkboxInput("adv_year_select_all", "Select/Deselect All Years", value = FALSE),
                           
                           fluidRow(
                             column(
                               width = 6,
                               numericInput("hr_weight", "HR Weight (FIP)", value = 13),
                               numericInput("bb_weight", "BB Weight (FIP)", value = 3),
                               numericInput("so_weight", "SO Weight (FIP)", value = 2),
                               numericInput("fip_constant", "FIP Constant", value = 3.1),
                               numericInput("fip_min_ip", "Min IP (FIP)", value = 10)
                             ),
                             column(
                               width = 6,
                               numericInput("k_pct_min_bf", "Min BF (K%)", value = 10),
                               numericInput("bb_pct_min_bf", "Min BF (BB%)", value = 10),
                               numericInput("k_bb_min_bb", "Min BB (K/BB)", value = 1),
                               numericInput("k_bb_min_ip", "Min IP (K/BB)", value = 10),
                               numericInput("babip_min_ip", "Min IP (BABIP)", value = 10)
                             )
                           ),
                          
                           br(),
                           actionButton("reset_adv_stats", "Reset")
                         )
                       ),
                       column(
                         width = 9,
                         box(
                           title = "Advanced Statistics Output",
                           solidHeader = TRUE,
                           status = "primary",
                           width = 12,
                           DTOutput("adv_stats_output")  
                         )
                       )
                     )
            ),
            
            tabPanel("Plot Tab", 
                     fluidRow(
                       column(
                         width = 4,
                         box(
                           title = "Filter Plot",
                           solidHeader = TRUE,
                           status = "info",
                           width = 12,
                           selectInput(
                             "plot_schools",
                             "Select School(s)",
                             choices = schools,
                             selected = first(schools)
                           ),
                         uiOutput("plot_var_selector")
                         )),
                       column(
                         width = 8,
                         box(
                           title = "Plot",
                           solidHeader = TRUE,
                           status = "primary",
                           width = 12,
                           plotlyOutput("analysis_plot")
                         )
                       )
                     )
            )
          )
        )
      ),
      
      tabItem(tabName = "ratings_tab",
              fluidRow(
                # Left Panel for inputs
                column(
                  width = 3,
                  box(width = 12, 
                      title = "Select Filters", 
                      status = "info", 
                      solidHeader = TRUE,
                      uiOutput("shared_rating_inputs"),
                      actionButton("update_ratings", "Recalculate Ratings", class = "btn-primary"),
                      br(), br(),
                      actionButton("reset_rating_inputs", "Reset Weights & Priors", class = "btn-secondary")
                  ),
                  
                  box(width = 12, title = "Prior Weights (Shrinkage Factors)", status = "info", solidHeader = TRUE,
                      fluidRow(
                        column(6,
                               lapply(sort(rating_stats$stat)[1:ceiling(length(rating_stats$stat)/2)], function(stat) {
                                 numericInput(
                                   inputId = paste0("prior_", stat),
                                   label = paste("Prior:", stat),
                                   value = rating_stats$prior_weight[rating_stats$stat == stat],
                                   step = 1
                                 )
                               })
                        ),
                        column(6,
                               lapply(sort(rating_stats$stat)[(ceiling(length(rating_stats$stat)/2) + 1):length(rating_stats$stat)], function(stat) {
                                 numericInput(
                                   inputId = paste0("prior_", stat),
                                   label = paste("Prior:", stat),
                                   value = rating_stats$prior_weight[rating_stats$stat == stat],
                                   step = 1
                                 )
                               })
                        )
                      )
                  ),
                  
                  box(width = 12, title = "Composite Rating Weights", status = "info", solidHeader = TRUE,
                      textOutput("weight_sum_warning"),
                      fluidRow(
                        column(6,
                               lapply(sort(rating_stats$stat)[1:ceiling(length(rating_stats$stat)/2)], function(stat) {
                                 numericInput(
                                   inputId = paste0("weight_", stat),
                                   label = paste("Weight:", stat),
                                   value = stat_weights[[paste0(stat, "_rating")]],
                                   step = 0.01, min = 0, max = 1
                                 )
                               })
                        ),
                        column(6,
                               lapply(sort(rating_stats$stat)[(ceiling(length(rating_stats$stat)/2) + 1):length(rating_stats$stat)], function(stat) {
                                 numericInput(
                                   inputId = paste0("weight_", stat),
                                   label = paste("Weight:", stat),
                                   value = stat_weights[[paste0(stat, "_rating")]],
                                   step = 0.01, min = 0, max = 1
                                 )
                               })
                        )
                      )
                  )
                  
                ),
                
                # Right Panel for outputs
                column(
                  width = 9,
                  tabBox(width = 12,
                         tabPanel("Player Ratings",
                                  fluidRow(
                                    box(
                                      width = 12,
                                      title = "Player Ratings",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      DTOutput("ratings_table"))
                                    )
                                  ),
                         tabPanel("Methodology",
                                  fluidRow(
                                    box(
                                      width = 12,
                                      title = "Methodology Data",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      DTOutput("methodology_table"))
                                    )
                                  ),
                         tabPanel("Info",
                                  fluidRow(
                                    box(
                                      width = 12,
                                      title = "Methodology Expanded",
                                      status = "primary",
                                      solidHeader = TRUE,
                                      HTML("
        <p>This player rating system evaluates NCAA Division I pitchers based on a weighted combination of performance metrics. The process includes:</p>
        <ul>
          <li><strong>Stat Selection:</strong> ERA, FIP, SO/9, BB/9, HR/9, WHIP, SO/W, K%, BB%.</li>
          <li><strong>League Averages:</strong> Computed by year to establish context.</li>
          <li><strong>Shrinkage:</strong> Each pitcher's stat is 'shrunk' toward the league average based on innings pitched and a prior weight.</li>
          <li><strong>Stat Rating:</strong> Pitchers are rated for each stat on a percentile basis (0–100 scale).</li>
          <li><strong>Composite Rating:</strong> Weighted average of individual stat ratings using user-defined weights.</li>
        </ul>
        <p>You can customize the weights and prior shrinkage values in the 'Methodology' tab.</p>
      ")
                                    )
                                  )
                         )
                         
                  )
                )
              )
      )
    )
  )
)

