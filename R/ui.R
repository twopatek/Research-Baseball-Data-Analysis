# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "NCAA Pitching Analysis Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Leaderboard", tabName = "leaderboard_tab"),
      menuItem("Data", tabName = "data_tab"),
      menuItem("Analysis", tabName = "analysis_tab")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "leaderboard_tab",
        fluidRow(
          box(
            title = "Filter by Minimum Innings Pitched",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            uiOutput("ip_slider_ui")
          )
        ),
        fluidRow(
          box(
            title = "Top 5 ERA (Team-Year)",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            DTOutput("top_era_teams")
          ),
          box(
            title = "Top 5 ERA (Player-Year)",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            DTOutput("top_era_players")
          )
        ),
        fluidRow(
          box(
            title = "Top 5 WHIP (Team-Year)",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            DTOutput("top_whip_teams")
          ),
          box(
            title = "Top 5 WHIP (Player-Year)",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            DTOutput("top_whip_players")
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
              selectInput(
                "data_schools",
                "Select School(s)",
                choices = schools,
                selected = "LSU",
                multiple = TRUE
              ),
              checkboxInput("data_school_select_all", "Select/Deselect All Schools", value = TRUE),
              selectInput(
                "years",
                "Select Year(s)",
                choices = seasons,
                selected = "2025",
                multiple = TRUE
              ),
              checkboxInput("year_select_all", "Select/Deselect All Years", value = TRUE),
              br(),
              actionButton("reset_data_table", "Reset Table")
            )
          ),
          column(
            width = 6,
            box(
              title = "Choose the minimum or maximum innings pitched",
              solidHeader = TRUE,
              status = "warning",
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
        "analysis_tab",
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
                           status = "primary",
                           width = 12,
                           selectInput(
                             "report_schools",
                             "Select School(s)",
                             choices = schools,
                             selected = "LSU",
                             multiple = TRUE
                           ),
                           checkboxInput("report_school_select_all", "Select/Deselect All Schools", value = TRUE),
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
                           status = "info",
                           width = 12,
                           DTOutput("analysis_report")
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
                           status = "warning",
                           width = 12,
                           selectInput(
                             "plot_schools",
                             "Select School(s)",
                             choices = schools,
                             selected = "LSU"
                           ),
                         uiOutput("plot_var_selector")
                         )),
                       column(
                         width = 8,
                         box(
                           title = "Plot",
                           solidHeader = TRUE,
                           status = "info",
                           width = 12,
                           plotlyOutput("analysis_plot")
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


