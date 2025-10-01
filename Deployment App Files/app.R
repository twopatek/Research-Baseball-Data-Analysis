# # Run the application
# app_dir <- file.path("Deployment App Files")

# source(file.path(app_dir, "global.R"), local = TRUE)
# source(file.path(app_dir, "ui.R"), local = TRUE)
# source(file.path(app_dir, "server.R"), local = TRUE)

# shinyApp(ui, server)

shiny::runApp("Deployment App Files")
