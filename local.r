
# Deploy RShiny app from GitHub
install.packages("shiny")
library(shiny)
runGitHub(repo = "TreeCensus_Dashboard", username = "namhaivu173", ref = "main")
