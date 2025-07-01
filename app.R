# Run the application
source("global.R")
source("ui.R")
source("server.R")
shinyApp(ui, server)

install.packages("http://cran.r-project.org/src/contrib/Archive/curl/curl_6.2.3.tar.gz", repos=NULL, type="source")

options(pkgType = "binary")

install.packages("rsconnect", type = "binary")
library(rsconnect)

deployApp()
