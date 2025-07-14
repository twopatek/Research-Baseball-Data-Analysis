# remotes::install_version("shinyWidgets", version = "0.7.6")

library(shiny)
library(shinyWidgets)
library(bslib)

options(shiny.launch.browser = TRUE)

ui <- fluidPage(
  theme = bs_theme(version = 4),  # ✅ force Bootstrap 4
  titlePanel("PickerInput Test Dashboard"),
  h3("Test pickerInput() inside a Bootstrap 4-compatible layout"),
  pickerInput(
    inputId = "test_picker",
    label = "Choose some letters:",
    choices = c("A", "B", "C", "D"),
    selected = "A",
    multiple = TRUE,
    options = list(
      `actions-box` = TRUE,
      `live-search` = TRUE,
      `selected-text-format` = "count > 2"
    )
  ),
  verbatimTextOutput("selected")
)

server <- function(input, output, session) {
  output$selected <- renderPrint({
    input$test_picker
  })
}

shinyApp(ui, server)

