# Run the application
source("global.R")
source("ui.R")
source("server.R")
shinyApp(ui, server)

# remotes::install_version("bslib", version = "0.4.2")
# remotes::install_version("shiny", version = "1.7.4")
# install.packages("rsconnect", type = "binary")
# library(rsconnect)
# 
# options(shinywidgetsBSVersion = 4)

deployApp()
