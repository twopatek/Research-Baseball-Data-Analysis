# Run the application
source("CopyOfglobal.R")
source("CopyOfui.R")
source("CopyOfserver.R")
shinyApp(ui, server)

deployApp()
