p <- c("tidyverse", "shiny", "leaflet", "httr2", "googlePolylines", "lpSolve", "shinydashboard", "plotly")
new.packages <- p[!(p %in% installed.packages()[, "Package"])]
if (length(new.packages)) {
    install.packages(new.packages, dependencies = TRUE, repos = "https://cloud.r-project.org")
}
