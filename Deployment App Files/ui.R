ui <- dashboardPage(
  dashboardHeader(title = "NCAA Pitching App"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Leaderboard", tabName = "leaderboard_tab"),
      menuItem("Player Ratings", tabName = "ratings_tab"),
      menuItem("Info", tabName = "info_tab")
    )
  ),
  dashboardBody(
    tabItems(
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
            title = "Top Strikeouts (Team-Year)",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            div(
              style = "height: 250px; overflow-y: scroll;",
              DTOutput("top_so_teams")
            )
          ),
          box(
            title = "Top Strikeouts (Player-Year)",
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
            title = "Top ERA (Team-Year)",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            div(
              style = "height: 250px; overflow-y: scroll;",
              DTOutput("top_era_teams")
            )
          ),
          box(
            title = "Top ERA (Player-Year)",
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
            title = "Top WHIP (Team-Year)",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            div(
              style = "height: 250px; overflow-y: scroll;",
              DTOutput("top_whip_teams")
            )
          ),
          box(
            title = "Top WHIP (Player-Year)",
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
        tabName = "ratings_tab",
        fluidRow(
          # Left Panel for inputs
          column(
            width = 3,
            box(
              width = 12,
              title = "Select Filters",
              status = "info",
              solidHeader = TRUE,
              uiOutput("shared_rating_inputs"),
              actionButton(
                "update_ratings",
                "Recalculate Ratings",
                class = "btn-primary"
              ),
              br(),
              br(),
              actionButton(
                "reset_rating_inputs",
                "Reset Weights & Priors",
                class = "btn-secondary"
              )
            ),
            box(
              width = 12,
              title = "Prior Weights (Shrinkage Factors)",
              status = "info",
              solidHeader = TRUE,
              fluidRow(
                column(
                  6,
                  lapply(
                    sort(rating_stats$stat)[
                      1:ceiling(length(rating_stats$stat) / 2)
                    ],
                    function(stat) {
                      numericInput(
                        inputId = paste0("prior_", stat),
                        label = paste("Prior:", stat),
                        value = rating_stats$prior_weight[
                          rating_stats$stat == stat
                        ],
                        step = 1
                      )
                    }
                  )
                ),
                column(
                  6,
                  lapply(
                    sort(rating_stats$stat)[
                      (ceiling(length(rating_stats$stat) / 2) + 1):length(
                        rating_stats$stat
                      )
                    ],
                    function(stat) {
                      numericInput(
                        inputId = paste0("prior_", stat),
                        label = paste("Prior:", stat),
                        value = rating_stats$prior_weight[
                          rating_stats$stat == stat
                        ],
                        step = 1
                      )
                    }
                  )
                )
              )
            ),

            box(
              width = 12,
              title = "Composite Rating Weights",
              status = "info",
              solidHeader = TRUE,
              textOutput("weight_sum_warning"),
              fluidRow(
                column(
                  6,
                  lapply(
                    sort(rating_stats$stat)[
                      1:ceiling(length(rating_stats$stat) / 2)
                    ],
                    function(stat) {
                      numericInput(
                        inputId = paste0("weight_", stat),
                        label = paste("Weight:", stat),
                        value = stat_weights[[paste0(stat, "_rating")]],
                        step = 0.01,
                        min = 0,
                        max = 1
                      )
                    }
                  )
                ),
                column(
                  6,
                  lapply(
                    sort(rating_stats$stat)[
                      (ceiling(length(rating_stats$stat) / 2) + 1):length(
                        rating_stats$stat
                      )
                    ],
                    function(stat) {
                      numericInput(
                        inputId = paste0("weight_", stat),
                        label = paste("Weight:", stat),
                        value = stat_weights[[paste0(stat, "_rating")]],
                        step = 0.01,
                        min = 0,
                        max = 1
                      )
                    }
                  )
                )
              )
            )
          ),

          # Right Panel for outputs
          column(
            width = 9,
            tabBox(
              width = 12,
              tabPanel(
                "Player Ratings",
                fluidRow(
                  box(
                    width = 12,
                    title = "Player Ratings",
                    status = "primary",
                    solidHeader = TRUE,
                    DTOutput("ratings_table")
                  )
                )
              ),
              tabPanel(
                "Info",
                fluidRow(
                  box(
                    width = 12,
                    title = "Methodology Expanded",
                    status = "primary",
                    solidHeader = TRUE,
                    HTML(
                      "
        <p>This player rating system evaluates NCAA Division I pitchers based on a weighted combination of performance metrics. The process includes:</p>
        <ul>
          <li><strong>Stat Selection:</strong> ERA, FIP, SO/9, BB/9, HR/9, WHIP, SO/W, K%, BB%.</li>
          <li><strong>League Averages:</strong> Computed by year to establish context.</li>
          <li><strong>Shrinkage:</strong> Each pitcher's stat is 'shrunk' toward the league average based on innings pitched and a prior weight.</li>
          <li><strong>Stat Rating:</strong> Pitchers are rated for each stat on a percentile basis (0–100 scale).</li>
          <li><strong>Composite Rating:</strong> Weighted average of individual stat ratings using user-defined weights.</li>
        </ul>
      "
                    )
                  )
                )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "info_tab",
        fluidRow(
          box(
            title = "Project Overview",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            HTML(
              "
              <p>This NCAA Pitching Dashboard is a personal, non-commercial project created to analyze and visualize trends in college baseball pitching statistics.</p>

  <p><strong>Project Goals:</strong></p>
  <ul>
    <li>Explore historical NCAA pitching data across multiple seasons.</li>
    <li>Provide an interactive dashboard for filtering, analysis, and visualization of player and team performance.</li>
  </ul>

  <p><strong>Data Methodology:</strong></p>
  <ul>
    <li>Data was manually compiled from publicly accessible team season pages available on a leading baseball statistics website.</li>
    <li>Files were standardized and cleaned locally before being included in the dashboard.</li>
  </ul>

  <p><em>This dashboard is for educational and demonstration purposes only. It is not affiliated with or endorsed by any official data provider or governing body.</em></p>
"
            )
          )
        )
      )
    )
  )
)
