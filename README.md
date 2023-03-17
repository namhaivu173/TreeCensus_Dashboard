<h1 align="center">
Building Tree Census Dashboard with R Shiny
</h1>
<p align="center">
<img src= "https://i.pinimg.com/originals/26/c2/ab/26c2ab08e01bb8205bf40502fb7aea6b.jpg" 
alt="" title="Image Source: https://www.pinterest.com/pin/137500594847520243/" width="65%" height="60%">
</p>
<p align="center">
<sup><i>(Image Source: https://www.pinterest.com/pin/137500594847520243/)</i></sup>
<!--- <sup><i><a href="https://www.pinterest.com/pin/137500594847520243">Image Source</a></i></sup> --->
</p>

The dashboard can be viewed in 2 ways:
- Online using Shinyapps.io: https://hainamvu.shinyapps.io/TreeCensus_Dashboard/
- Remotely using RStudio: 
  - Install & load the "shiny" package: ``install.packages("shiny")`` and ``library(shiny)``
  - Run Shiny app on GitHub: ``shiny::runGitHub(repo = "TreeCensus_Dashboard", username = "namhaivu173", ref = "main")``

<p>
<b><i>Note: Due to the limited memory size allowed on Shinyapps.io, only a portion of the original data set is used for building this dashboard.</i></b>
</p>

## I. Data set:
- Downloaded from NYC Open Data: https://data.cityofnewyork.us/Environment/2015-Street-Tree-Census-Tree-Data/uvpi-gqnh
- The 2015 New York (NY) tree census data set contains various features of trees planted in NY, including but not limited to location details, trunk diameter, perception of health, etc.

## II. Main goals:

- Analyze and explore interesting insights about the tree census dataset via effective visualizations
- Practice designing R Shiny applications and deploy dashboard to shinyapps.io

## III. Files shared:

- .zip: contains the original data set from NYC Open Data
- .pdf: contains variable dictionary for the data set
- Compressed_data.ipynb: contains script that produce a sample data set from the original data set
- .csv: a portion of the original data set produced using the Python script
- app.r: contains script for the R Shiny app
- local.r: contains script that deploys the R Shiny app by connecting to GitHub repo
