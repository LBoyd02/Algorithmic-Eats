<<<<<<< HEAD
p <- c("tidyverse", "shiny", "leaflet", "httr2", "googlePolylines", "lpSolve", "shinydashboard", "plotly", "s2", "units", "raster", "sf")
new.packages <- p[!(p %in% installed.packages()[, "Package"])]
if (length(new.packages)) {
    install.packages(new.packages, dependencies = TRUE, repos = "https://cloud.r-project.org")
=======
options(repos = c(CRAN = "https://cloud.r-project.org"))

p <- c("tidyverse","shiny","leaflet","httr2","googlePolylines","lpSolve","shinydashboard","plotly", "s2", "raster", "sf", "units")

to_install <- setdiff(p, rownames(installed.packages()))
if (length(to_install)) {
  install.packages(to_install, dependencies = c("Depends","Imports","LinkingTo"))
>>>>>>> 7ac5334db12a56425074c523eaf1e676b63f2581
}

missing <- setdiff(p, rownames(installed.packages()))
if (length(missing)) stop("Failed to install: ", paste(missing, collapse = ", "))
