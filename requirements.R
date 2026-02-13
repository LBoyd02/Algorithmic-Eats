options(repos = c(CRAN = "https://cloud.r-project.org"))

p <- c("tidyverse","shiny","leaflet","httr2","googlePolylines","lpSolve","shinydashboard","plotly")

to_install <- setdiff(p, rownames(installed.packages()))
if (length(to_install)) {
  install.packages(to_install, dependencies = c("Depends","Imports","LinkingTo"))
}

missing <- setdiff(p, rownames(installed.packages()))
if (length(missing)) stop("Failed to install: ", paste(missing, collapse = ", "))
