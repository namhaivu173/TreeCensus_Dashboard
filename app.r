
#-------------------------------------------
# Libraries installation:
library(ggplot2)
library(DescTools)
library(car)
library(plyr)
library(knitr)
library(tidyverse)
library(RColorBrewer)
library(ggpubr)
library(psych)
library(treemap)
library(gridExtra)
library(shiny)
library(viridis)
library(shinydashboard)
library(timeDate)
library(rsconnect)
library(shinyWidgets)

# Find which loaded packages are not used
# used.functions <- NCmisc::list.functions.in.file(filename = "app.R", alphabetic = FALSE)
# used.packages <- used.functions |> names() |> grep(pattern = "package:", value = TRUE)
# used.packages <- unique(gsub("\")","", sub(".*package:", "", used.packages)))
# unused.packages <- list.of.packages[!(list.of.packages %in% used.packages)]

# Detach unused packages
# detach_package <- function(pkg, character.only = FALSE)
# {
#   if(!character.only)
#   {
#     pkg <- deparse(substitute(pkg))
#   }
#   search_item <- paste("package", pkg, sep = ":")
#   while(search_item %in% search())
#   {
#     detach(search_item, unload = TRUE, character.only = TRUE)
#   }
# }

#lapply(unused.packages, detach_package, character.only = TRUE)

#-------------------------------------------

# Import data
#df_tree <- read.csv(unz("2015_Street_Tree_Census_-_Tree_Data_sample.zip", "2015_Street_Tree_Census_-_Tree_Data_sample.csv"))
#df_tree1 <- sample_n(df_tree, size=50000, random_state=99)
df_tree <- read.csv("2015_Street_Tree_Census_-_Tree_Data_sample.csv",stringsAsFactors=FALSE)


# Create datetime features
df_census <- na.omit(df_tree)
df_census$created_at <- as.Date(df_census$created_at,format="%Y-%m-%d")
df_census$Year <- as.numeric(format(df_census$created_at,"%Y"))
df_census$Month <- as.numeric(format(df_census$created_at,"%m"))
#df_census$Month_Yr <- with(df_census, sprintf("%02d-%d", Month, Year))
df_census$year_month <- as.Date(timeFirstDayInMonth(df_census$created_at))

# Rename values of categorical features
df_census$health[df_census$health == ""] <- "Dead/Stump"
df_census$sidewalk[df_census$sidewalk == ""] <- "Not Applicable"
df_census$guards[df_census$guards == ""] <- "Not Applicable"
df_census$spc_common[df_census$spc_common == ""] <- "Others"
df_census$problems[df_census$problems == ""] <- "Others"
df_census$steward[df_census$steward == ""] <- "Not Applicable"
df_census$health <- factor(df_census$health, levels = c("Dead/Stump","Poor","Fair","Good"))

# Create tree diameter feature
df_census$diam <- df_census$tree_dbh + df_census$stump_diam
df_census <- subset(df_census, 
                    select = c(created_at, tree_dbh, curb_loc, status, user_type, health, zip_city, spc_common, year_month, borough, problems, guards, steward, diam, nta_name))

# Define %notin% function
`%notin%` <- Negate(`%in%`)

# Function that moves a row to last of table 
move_to_last <- function(df, n) df[c(setdiff(seq_len(nrow(df)), n), n), ]

# Remove unused data
rm(df_tree)
#rm(df_tree1)

#-------------------------------------------
# UI section:

css <- HTML("
h4{font-size: 15px;
   text-indent: 2em;
   padding-left: -2em;}
")

# .bs-deselect-all{display: none;}
# .bs-select-all{width: 100%;}

ui <- dashboardPage(
  dashboardHeader(title = "NYC Tree Census"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("First Dashboard", tabName = "first_app"),
      menuItem("Second Dashboard", tabName = "second_app"),
      menuItem("Third Dashboard", tabName = "third_app")
    )
  ),
  dashboardBody(
    tags$head(tags$style(css)),
    tabItems(
      tabItem(
        
        #-------
        # Tab 1:
        tabName = "first_app",
              titlePanel(
                h1("2015 NYC Tree Census Dashboard", align = "center", style='color:white;background-color:forestgreen;')
              ),
              
              # Sidebar with a slider input for number of bins
              sidebarLayout(
                sidebarPanel(
                  dateRangeInput(inputId = "dateRange",  
                                 label = "Select Time Range", 
                                 start = "2015-05-19",
                                 end = "2016-10-05"
                  ),
                  # Select borough
                  hr(),
                  pickerInput("borough","Borough", 
                              choices=list("Bronx" = "Bronx",
                                           "Brooklyn" = "Brooklyn",
                                           "Manhattan" = "Manhattan",
                                           "Queens" = "Queens",  
                                           "Staten Island" = "Staten Island"), 
                              options = list(`actions-box` = TRUE),
                              selected = c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island"),
                              multiple = T),
                  # Select health
                  hr(),
                  pickerInput("tree_health","Tree health", 
                              choices=list("Dead/Stump" = "Dead/Stump",
                                           "Poor" = "Poor",
                                           "Fair" = "Fair",
                                           "Good" = "Good"), 
                              options = list(`actions-box` = TRUE),
                              selected = c("Good","Fair","Poor","Dead/Stump"),
                              multiple = T),
                  # Select curb
                  hr(),
                  pickerInput("Curb","Location of tree with respect to the curb", 
                              choices=list("On Curb" = "OnCurb",
                                           "Offset From Curb" = "OffsetFromCurb"), 
                              options = list(`actions-box` = TRUE),
                              selected = "OnCurb",
                              multiple = T),
                  # Select user type
                  hr(),
                  pickerInput("user_type","Users who collected tree data", 
                              choices=list("NYC Parks Staff" = "NYC Parks Staff",
                                           "Trees Count Staff" = "TreesCount Staff",
                                           "Volunteer" = "Volunteer"), 
                              options = list(`actions-box` = TRUE),
                              selected = "NYC Parks Staff",
                              multiple = T),
                ),
                
                # Show a plot of the generated distribution
                mainPanel( 
                  uiOutput("img1"), #img(src = "www/Tree.jpg", height = 100, width = 600), 
                  tabsetPanel(
                    
                    # Tab panel 1
                    tabPanel("About", h1(HTML(paste0("<b>","Introduction","</b>")), align = "center",style = "color: #906060"),
                             h4(textOutput("textDisplay1"), align="justify", style = "color: black"),
                             
                             h3(HTML("<b>Tab Information:</b>"), align = "left", style = "color: dodgerblue" ), 
                             div(" 1) Overview: Presents a quick overview of tree density in NYC", style = "color: forestgreen"), 
                             div(" 2) Tree Species: Shows top 10 species in each Borough", style = "color: forestgreen"), 
                             div(" 3) Tree Health: Shows the tree health status based on the percentage", style = "color: forestgreen"),  
                             div(" 4) Tree Problem: Shows top problems that are impacting the tree health", style = "color: forestgreen"), 
                             div(" 5) Tree Protection: Shows the best measure to be taken to ensure trees' good health", style = "color: forestgreen"),  
                             div(" 6) Summary: Shares summarized insights of the dashboard", style = "color: forestgreen"), 
                             
                             h3(HTML("<b>Message to remember:</b>"),align = "left", style = "color: #D21500"),
                             uiOutput("img2"), #img(src = "maxresdefault.jpg", height = 200, align ="center",width = 600),
                             h5(HTML(paste0("<b>","***PLANT TREES, SAVE LIVES***","</b>")),align = "center"),
                             hr(),
                             # Disclaimer
                             h5(htmlOutput("disclaimer1"), align="center", style = "color: black"),
                    ),
                    # Tab panel 2
                    tabPanel("Overview", helpText("This section is to show the count of various trees present at different parts of New York city. The tables below contain such information:"),
                             box(title = "Count of trees by borough",
                                 status = "primary", solidHeader = T,
                                 collapsible = T, width = 12,
                                 column(12, align="center", tableOutput('introduction0'))),
                             box(title = "Count of trees by zip area (show top 10 max)",
                                 status = "primary", solidHeader = T,
                                 collapsible = T, width = 12,
                                 column(12, align="center", tableOutput('introduction'))),
                             box(title = "Top 5 streets with the most number of trees",
                                 status = "primary", solidHeader = T,
                                 collapsible = T, width = 12,
                                 column(12, align="center", tableOutput('introduction1'))),
                             box(title = "Count of trees by status & health conditions",
                                 status = "primary", solidHeader = T,
                                 collapsible = T, width = 12,
                                 column(12, align="center", tableOutput('introduction2'))),
                             ),
                    # Tab panel 3
                    tabPanel("Tree Species",
                             box(title = "Top 10 tree species by count",
                                 status = "primary", solidHeader = T,
                                 collapsible = F, width = 12,
                                 column(12, align="center", plotOutput("Spcbarplot", width="100%")))
                             ),
                    # Tab panel 4
                    tabPanel("Tree Health",
                             box(title = "Histogram showing tree diameter by health conditions",
                                 status = "primary", solidHeader = T,
                                 collapsible = F, width = 12,
                                 column(12, align="center", plotOutput("treeHealth", width="100%")))
                             ),
                    # Tab panel 5
                    tabPanel("Tree Problem",
                             box(title = "Top 10 problems observed in trees",
                                 status = "primary", solidHeader = T,
                                 collapsible = F, width = 12,
                                 column(12, align="center", plotOutput("probTreeHealth", width="100%", height="600px")))
                             ),
                    # Tab panel 6
                    tabPanel("Tree Protection",
                             box(title = "Feedback on the guards of trees",
                                 status = "primary", solidHeader = T,
                                 collapsible = T, width = 12,
                                 column(12, align="center", plotOutput("measureTreeHealth"))),
                             box(title = "Number of unique signs of stewardship observed on tree",
                                 status = "primary", solidHeader = T,
                                 collapsible = T, width = 12,
                                 column(12, align="center", plotOutput("measureTreeHealth2")))
                             ),
                    # Tab panel 7
                    tabPanel("Summary", h1(HTML(paste0("<b>","Summary","</b>")), align = "center",style = "color: #906060"),
                             h4(textOutput("textDisplay2"), align="justify", style = "color:black"),
                             uiOutput("img3")) #img(src = "save_image.jpg", height = 200, align ="center",width = 500))
                  )
                )
              )
      ),
      #-------
      # Tab 2:
      tabItem(tabName = "second_app",
              titlePanel(
                h1("2015 NYC Tree Census Dashboard", align = "center", style='color:white;background-color:forestgreen;')
              ),
              headerPanel(title = h1("Tree information among the top 5 areas with the most trees",align="center")),
              
              fluidRow(
                align="center",
                # Select date
                sliderInput("created_at","Select Time Range",
                            min=as.Date("2015-05-19","%Y-%m-%d"),
                            max=as.Date("2016-10-05","%Y-%m-%d"),
                            value=c(as.Date("2015-06-01","%Y-%m-%d"),as.Date("2015-09-01","%Y-%m-%d")),
                            timeFormat="%Y-%m")),
              
              # Print filtered selection
              fluidRow(
                align="center",
                h3(htmlOutput("Date"), align="center")),
              
              # Show treemap
              fluidRow(
                box(
                  title="Treemap of major areas & tree health",
                  width = 12,
                  status = "primary",
                  solidHeader = T,
                  collapsible = T,
                  plotOutput("treemap", height="500px"))
              ),
              
              # Show count by user
              fluidRow(
                box(
                  title="Count of trees by data collector group",
                  width = 12,
                  status = "primary",
                  solidHeader = T,
                  collapsible = T,
                  plotOutput("user_zip", height="500px"))
              ),
              
              # Disclaimer
              fluidRow(
                align="center",
                h5(htmlOutput("disclaimer2"), align="center", style = "color:black")),
      ),
      #-------
      # Tab 3:
      tabItem(tabName = "third_app",
              titlePanel(
                h1("2015 NYC Tree Census Dashboard",align = "center", style='color:white;background-color:forestgreen;')),
              
              # Box with stats
              fluidRow(
                valueBoxOutput("value1",width = 4),
                valueBoxOutput("value2",width = 4),
                valueBoxOutput("value3",width = 4)
              ),
              
              fluidRow(
                column(12,div(p(" "),style="color:white;"))
              ),
              sidebarLayout(
              sidebarPanel(
                
                # Select borough
                pickerInput("borough2","Borough", 
                            choices=list("Bronx" = "Bronx",
                                         "Brooklyn" = "Brooklyn",
                                         "Manhattan" = "Manhattan",
                                         "Queens" = "Queens",  
                                         "Staten Island" = "Staten Island"), 
                            options = list(`actions-box` = TRUE),
                            selected = c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island"),
                            multiple = T),
                hr(),
                # Select health
                pickerInput("health2","Tree health", 
                            choices=list("Dead/Stump" = "Dead/Stump",
                                         "Poor" = "Poor",
                                         "Fair" = "Fair",
                                         "Good" = "Good"), 
                            options = list(`actions-box` = TRUE),
                            selected = c("Good","Fair","Poor","Dead/Stump"),
                            multiple = T),
                hr(),
                # Select steward
                pickerInput("steward","Number of stewardship signs observed", 
                            choices=list("N/A" = "Not Applicable",
                                         "None" = "None",
                                         "1 or 2" = "1or2",
                                         "3 or 4" = "3or4",
                                         "4 or more" = "4orMore"), 
                            options = list(`actions-box` = TRUE),
                            selected = c("Not Applicable", "None", "1or2", "3or4", "4orMore"),
                            multiple = T)
              ),
              
              mainPanel(
                # Show % stacked bar
                fluidRow(
                  box(
                    title = "Percent stacked bar chart of top 5 populated species by health conditions",
                    width = 12,
                    status = "primary",
                    solidHeader = T,
                    collapsible = T,
                    #background = "light-blue",
                    plotOutput("chart1", height="500px")
                  )
                  ),
                # Show time series
                fluidRow(
                  box(
                    title = "Average diameter by tree status",
                    width = 12,
                    status = "primary",
                    solidHeader = T,
                    collapsible = T,
                    #background = "light-blue",
                    plotOutput("chart2", height="500px")
                  )
                  ),
                # Disclaimer
                fluidRow(
                  align="center",
                  h5(htmlOutput("disclaimer3"), align="center", style = "color:black")),
              )
              )
      )
    )
  )
)

#-------------------------------------------
# Server section:

server <-
  
  function(input,output,session)
  {
    #-------
    # Tab 1:
    
    df_census1 <- df_census
    makeReactiveBinding("df_census1")
    
    # reactive df
    finalTreeData <- reactive({
      req(input$dateRange, input$borough, input$Curb, input$user_type, input$tree_health)
      df_census1 <- filter(df_census, 
                           curb_loc %in% input$Curb & 
                           user_type %in% input$user_type & 
                           health %in% input$tree_health & 
                           borough %in% input$borough &
                           between(as.Date.character(created_at,"%Y-%m-%d"), 
                                   as.Date.character(input$dateRange[1],"%Y-%m-%d"), 
                                   as.Date.character(input$dateRange[2],"%Y-%m-%d")))
      return(df_census1)
    })
    # df for species plot
    df_Spcbarplot <- reactive({
      spc_freq <- data.frame(plyr::count(finalTreeData(), "spc_common"))
      spc_freq <- spc_freq[!(spc_freq$spc_common %in% "Others"),]
      spc_freq <- head(spc_freq[order(-spc_freq$freq),],10)
      return(spc_freq)
    })
    
    # df for health plot
    df_treeHealth <- reactive({
      health1 <- dplyr::count(finalTreeData(), health, sort = TRUE)
      piepercent <- round(100*health1$n/sum(health1$n), 1)
      health_df <- data.frame(health1,piepercent)
      return(health_df)
    })
    
    # df for problem plot
    df_probTreeHealth <- reactive({
      health_df1 <- data.frame(plyr::count(finalTreeData(), "problems"))
      problems_list <- c('Others','None')
      health_df1 <- health_df1[!(health_df1$problems %in% problems_list),]
      health_df1 <- head(health_df1[order(-health_df1$freq),],10)
      return(health_df1)
    })
    
    # df1 for measure plot
    df_measureTreeHealth <- reactive({
      health_df2 <- data.frame(plyr::count(finalTreeData(), c("guards")))
      health_df2 <- health_df2[!(health_df2$guards %in% "None"),]
      return(health_df2)
    })
    
    # df2 for measure plot
    df_measureTreeHealth2 <- reactive({
      health_df3 <- data.frame(plyr::count(finalTreeData(), c("steward")))
      health_df3 <- health_df3[!(health_df3$steward %in% "None"),]
      return(health_df3)
    })
    
    # text display in "about" tab
    output$textDisplay1 <- renderText({
      paste0('This project covers the analysis of the 2015 New York (NY) tree census data set, which contains various features of trees planted in NY including but not limited to location details, diameter, health conditions, etc. This data was collected by volunteers and staff organized by NYC Parks & Recreation and partner organizations. The tree census aids in the study of street tree density over a period of time, and suggests actionable insights for the city council to consider. The objective of this dashboard is to analyze and explore deeper insights about the tree census dataset through effective visualizations.')
    })

    # text display in "summary" tab
    output$textDisplay2 <- renderText({
      paste0("Planting trees in a hustling city like New York From have countless advantages, such as improving the air quality, beautifying the city scenary and mitigating climate change effects. By collecting information on tree count, health, problems, and protection, city officials can make informed decisions on how to best manage and maintain the urban forest. The data can also help identify areas where trees are lacking and guide future planting efforts. Furthermore, a healthy urban forest can improve air quality, reduce the urban heat island effect, and provide habitat for wildlife. By investing in a tree census survey, New York can ensure the longevity and sustainability of its urban forest for years to come."
      )
    })
    
    # disclaimer notes dashboard 1
    output$disclaimer1 <- renderText({
      paste0('<b><i>','Note: Due to the limited memory size allowed on Shinyapps.io, only a portion of the original data set is used for building this dashboard.','</i></b>')
    })
    
    # disclaimer notes dashboard 2
    output$disclaimer2 <- renderText({
      paste0('<b><i>','Note: Due to the limited memory size allowed on Shinyapps.io, only a portion of the original data set is used for building this dashboard.','</i></b>')
    })
    
    # disclaimer notes dashboard 3
    output$disclaimer3 <- renderText({
      paste0('<b><i>','Note: Due to the limited memory size allowed on Shinyapps.io, only a portion of the original data set is used for building this dashboard.','</i></b>')
    })
    
    # build species plot
    output$Spcbarplot <- renderPlot({
      p <- ggplot(df_Spcbarplot(), aes(x = reorder(spc_common,freq, sum), 
                                              y = freq, 
                                              label=freq, 
                                              fill=freq)) +
        geom_bar(stat = "identity") + labs(x="Species",y="Frequency") +
        geom_text(size=4, hjust = 1.2, color="black") + coord_flip() + # fontface="bold"
        theme_light() +
        theme(legend.position="right", 
              axis.text=element_text(size=14),
              axis.title=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major.y = element_blank()) + 
        xlab("Top 10 Species\n") +
        ylab("\nCount") +
        #scale_fill_viridis(discrete=F, option="C", direction=-1) +
        scale_fill_gradient(low = "grey", high = "darkgreen") +
        labs(fill="Frequency")
      return(p)
    }) 
    
    # build health plot
    output$treeHealth <-renderPlot({
      p <- ggplot(finalTreeData(), aes(x=diam, fill=reorder(health, new.order=c("Good","Fair","Poor","Dead/Stump")))) +
        geom_histogram(alpha=0.5, position = "identity") + # color="#e9ecef"
        #scale_fill_manual(values=c("#69b3a2", "#404080")) +
        theme_light() +
        theme(legend.position="right", 
              axis.text=element_text(size=14),
              axis.title=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank()) + 
        scale_fill_brewer(palette = "Dark2") +
        xlab("\nTree Diameter (in)") +
        xlim(0,50) +
        ylab("Count\n") +
        labs(fill="Tree Health")
      return(p)
      
    })
    
    # build problem plot
    output$probTreeHealth <- renderPlot({

      p <-  ggplot(df_probTreeHealth(), aes(x=reorder(problems,-freq), y=freq/1000, fill=freq/1000)) +
          geom_bar(stat="identity") +
          geom_text(aes(label = round(freq/1000,1)),col="black",size=4,vjust=-0.5) +
          theme_light() +
          theme(legend.position="right", 
                axis.text=element_text(size=14),
                axis.title=element_text(size=14),
                legend.text=element_text(size=13),
                legend.title=element_text(size=13),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.y = element_blank(),
                axis.text.x = element_text(angle=45, vjust=1, hjust=1)) + 
          # scale_fill_gradient2(low = "lightgray", mid="lightskyblue", high = "dodgerblue4", midpoint=median(head(health_df1$freq,10))/1000) +
          scale_fill_viridis(discrete=F, option="D", direction=-1) +
          labs(x="\nTop 10 Problems",y="Frequency (in thousands)\n", fill="Unit:\nthousands")
      return(p)
      
    })
    
    # build measure plot 1
    output$measureTreeHealth <- renderPlot({
      
      p <- ggplot(df_measureTreeHealth(), aes(x = reorder(guards,freq), 
                                          y = freq, 
                                          label=freq, 
                                          fill=freq)) +
        geom_bar(stat = "identity") + labs(x="Species",y="Frequency") +
        geom_text(size=4, hjust = 1.2, color="black") + coord_flip() + # fontface="bold"
        theme_light() +
        theme(legend.position="right", 
              axis.text=element_text(size=14),
              axis.title=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.x = element_blank()) + 
        xlab("Tree Guards' Feedback\n") +
        ylab("\nFrequencies") +
        #scale_fill_viridis(discrete=F, option="C", direction=-1) +
        #scale_fill_gradient(low = "grey", high = "darkgreen") +
        scale_fill_gradient2(low = "ivory2", mid="lightsalmon", high = "darkorange2", midpoint=mean(df_measureTreeHealth()$freq)) +
        labs(fill="Frequency")
      
      return(p)
      
    })
    
    # build measure plot 2
    output$measureTreeHealth2 <- renderPlot({
      p <- ggplot(df_measureTreeHealth2(), aes(x = reorder(steward,freq), 
                                               y = freq, 
                                               label=freq, 
                                               fill=freq)) +
        geom_bar(stat = "identity") + labs(x="Species",y="Frequency") +
        geom_text(size=4, hjust = 1.2, color="black") + coord_flip() + # fontface="bold"
        theme_light() +
        theme(legend.position="right", 
              axis.text=element_text(size=14),
              axis.title=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.x = element_blank()) + 
        xlab("Number of Stewardship Observations\n") +
        ylab("\nFrequencies") +
        scale_fill_gradient2(low = "ivory2", mid="lightpink1", high = "pink4", midpoint=mean(df_measureTreeHealth2()$freq)) +
        labs(fill="Frequency")
      
      return(p)
      
    })
    
    # build count by borough
    output$introduction0 <- renderTable({
      new_tabl <- as.data.frame(table(finalTreeData()$borough))
      colnames(new_tabl) <- c("Borough","Count")
      new_tabl <- new_tabl[order(-new_tabl$Count),]
      new_tabl$Proportion <- round(new_tabl$Count/sum(new_tabl$Count),2)
      new_tabl
    }, align='l', striped=T, bordered=T)
    
    # build count by zip area
    output$introduction <- renderTable({
      new_tabl <- as.data.frame(table(finalTreeData()$borough,finalTreeData()$zip_city))
      colnames(new_tabl) <- c("Borough","Zip Area Name","Count")
      new_tabl <- new_tabl[order(-new_tabl$Count),]
      new_tabl$Proportion <- round(new_tabl$Count/sum(new_tabl$Count),2)
      return(head(new_tabl,10))
    }, align='l', striped=T, bordered=T)
    
    # build count by street
    output$introduction1 <- renderTable({
      new_tabl <- as.data.frame(table(finalTreeData()$borough,finalTreeData()$nta_name))
      colnames(new_tabl) <- c("Borough","Street Name","Top 5 Count")
      new_tabl <- head(new_tabl[order(-new_tabl$`Top 5 Count`),],5)
      new_tabl
    }, align='l', striped=T, bordered=T)

    # build count by health & status
    output$introduction2 <- renderTable({
      health_tabl<-as.data.frame(table(finalTreeData()$status, finalTreeData()$health))
      colnames(health_tabl)<-c("Tree Status","Tree Health","Count")
      health_tabl <- health_tabl[order(-health_tabl$Count),]
      health_tabl <- filter(health_tabl, Count != 0)
      health_tabl$Proportion <- round(health_tabl$Count/sum(health_tabl$Count),2)
      health_tabl
    }, align='l', striped=T, bordered=T)
    
    # import images
    output$img1 <- renderUI({
      tags$img(src = "https://www.scenic.org/wp-content/uploads/2019/10/tree-preservation-slide.jpg", height=150, width="100%")
    })
    
    output$img2 <- renderUI({
      tags$img(src = "https://youmatter.world/app/uploads/sites/2/2018/10/climate-change-definition-meaning.jpg", height=300, width="100%")
    })
    
    output$img3 <- renderUI({
      tags$img(src = "https://media.architecturaldigest.com/photos/56a177c6f62777972f2fe407/16:9/w_3104,h_1746,c_limit/million-trees-new-york-01.jpg", height=250, width="100%")
    })
    
    
    #-------
    # Tab 2:
    
    # reactive df
    Topzip_new <- reactive({
      req(input$created_at)
      Topzip_df <- filter(df_census, between(created_at, input$created_at[1], input$created_at[2]))
      top5zip <- names(head(sort(table(Topzip_df$zip_city),decreasing=T),5))
      Topzip_df <- Topzip_df[Topzip_df$zip_city %in% top5zip, ]
      return(Topzip_df)
    })
    
    # df for treemap
    Topzip_freq <- reactive({
      Topzip_freq1 <- Topzip_new() %>% 
        group_by(zip_city, health) %>%
        dplyr::summarise(Frequency = n(),.groups = 'drop')
      Topzip_freq1$labels = paste0(Topzip_freq1$health, "\n", Topzip_freq1$Frequency)
      return(Topzip_freq1)
    })
    
    # df for top areas plot
    Topzip_freq2 <- reactive({
      Topzip_freq2 <- Topzip_new() %>% 
        group_by(zip_city, user_type) %>%
        dplyr::summarise(Frequency = n(),.groups = 'drop')
      return(Topzip_freq2)
    })
    
    # show data filtered time
    output$Date  <- {(
      renderText(paste0("Data filtered from ","<b>",as.character(input$created_at[1]),"</b>",
                        " to ","<b>",as.character(input$created_at[2]),"</b>"))
    )}
    
    # build treemap
    output$treemap <- renderPlot(
      
      treemap(Topzip_freq(), index=c("zip_city","labels"), vSize="Frequency", type="index", #vColor="health",
              fontsize.labels=c(16,12),                # size of labels
              fontcolor.labels=c("white","black"),     # Color of labels
              fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic
              bg.labels=0,                             # Background color of labels
              align.labels=list(
                c("center", "center"), 
                c("right", "bottom")),                 # Where to place labels in the rectangle
              overlap.labels=0,                      
              inflate.labels=F,
              position.legend="none")
      
    )
    
    # build top areas plot
    output$user_zip <- renderPlot(
      ggplot(Topzip_freq2(),aes(x=reorder(zip_city,-Frequency), y=Frequency, fill= reorder(user_type,-Frequency))) +
        geom_bar(stat="identity", position = "dodge") +
        theme_light() +
        labs(x= "\nTop 5 areas with the most trees",
             y="Tree Count\n",
             title="",
             fill="Data Collector") +
        theme(axis.text=element_text(size=14),
              axis.title=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank()) +
        scale_fill_brewer(palette="Set1") +
        geom_text(aes(label=paste0(Frequency,"\n")), size=3.5, position = position_dodge(width=0.9))
    )
    
    #-------
    # Tab 3:
    
    # reactive df
    df_new <- reactive({
      req(input$borough2,input$health2)
      df_census2 <- filter(df_census,borough %in% input$borough2 & health %in% input$health2 & steward %in% input$steward)
      return(df_census2)
    })
    
    # df for top species % stacked plot
    df_top5_spec <- reactive({
      top5species <- try(names(head(sort(table(df_new()$spc_common),decreasing=T),5)),silent=T)
      df_top5spec <- df_new()[df_new()$spc_common %in% top5species, ]
      df_top5_spec <- df_top5spec %>% 
        group_by(spc_common, health) %>%
        dplyr::summarise(tree_count = n(),.groups = 'drop')
      return(df_top5_spec)
    })
    
    # df for time series
    df_status <- reactive({
      df_new() %>%
        group_by(status,year_month) %>%
        dplyr::summarise(tree_diam = mean(diam),.groups="drop")
    })
    
    # stats box 1
    output$value1 <- renderValueBox({
      valueBox(
        value=format(nrow(df_new()),big.mark=","),width=12,
        subtitle = paste("The total number of street trees in", paste(input$borough2, collapse = ", ")),
        color = "teal")
      #icon = icon("stats",lib="glyphicon"),
      #color = "light-blue")
    })
    
    # stats box 2
    output$value2 <- renderValueBox({
      valueBox(
        value=format(length(unique(df_new()$spc_common)),big.mark=","),width=12,
        subtitle = paste("The total number of unique species in", paste(input$borough2, collapse = ", ")),
        color = "orange")
      #icon = icon("stats",lib="glyphicon"),
      #color = "navy")
    })
    
    # stats box 3
    output$value3 <- renderValueBox({
      valueBox(
        value=paste(round(mean(as.numeric(df_new()$diam)),1),"in"),width=12,
        subtitle = paste("The average diameter of trees in", paste(input$borough2, collapse = ", ")),
        color = "black")
      #icon = icon("stats",lib="glyphicon"),
      #color = "yellow")
    })
    
    # build top species % stacked plot
    output$chart1 <- renderPlot({
      ggplot(df_top5_spec(),aes(x=reorder(spc_common,-tree_count,sum), y=tree_count, 
                                fill= reorder(health, new.order=c("Good","Fair","Poor","Dead/Stump")))) +
        geom_bar(stat="identity", position = "fill") +
        theme_light() +
        theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1),
              axis.text=element_text(size=14),
              axis.title=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.y = element_blank()) +
        scale_fill_brewer(palette="Dark2") +
        labs(x= "\nTop 5 populated species",
             y="Tree Proportion\n",
             title="",
             fill="Tree Health") +
        #ylim(0,1) +
        stat_summary(fun = sum, aes(label = ..y.., group = spc_common), 
                     geom = "text", position = position_fill(vjust = 1.05), size=4) #vjust = -0.5
    })
    
    # build time series
    output$chart2 <- renderPlot({
      ggplot(df_status()) +
        geom_line(aes(x=year_month, y=tree_diam, col=status), linewidth=1.5) +
        theme_light() +
        theme(axis.text=element_text(size=14),
              axis.title=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.minor.y = element_blank()) +
        labs(x="\nYear & Month",
             y="Average Diameter (inch)\n",
             col = "Tree Status")
    })
  }

shinyApp(ui = ui, server = server)

