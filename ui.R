# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "LSU Pitching Analysis Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "data_tab"),
      menuItem("Analysis", tabName = "analysis_tab")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "data_tab",
        fluidRow(
          column(
            width = 4,
            selectInput(
              "years",
              "Select Year(s)",
              choices = seasons,
              selected = "2025",
              multiple = TRUE
            ),
            checkboxInput("input_select_all", "Select/Deselect All Years", value = TRUE)
          )
        ),
        uiOutput("ip_slider"),
        DTOutput("raw_data")
      ),
      tabItem(
        "analysis_tab",
        fluidRow(
          tabBox(
            width = 12,
            tabPanel("Report Tab", 
              fluidRow(
                column(
                  width = 4,
                  uiOutput("report_var_selector"),
                  actionButton("summary_stats", "Calculate Summary Stats"),
                  br(), br(), 
                  actionButton("reset_table", "Reset Table")
                ),
                column(
                  width = 8,
                  DTOutput("analysis_report")
                  )
                )
              ),
            tabPanel("Plot Tab", 
                     fluidRow(
                       column(
                         width = 4,
                         uiOutput("plot_var_selector")
                       ),
                       column(
                         width = 8,
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
  


