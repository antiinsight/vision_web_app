############################################################
#
#   VIZN - Visual Interrogation of Zebrafish MaNipulatons
#
#     Created by Anthony Scott
#       May 28th, 2015
#
#     ui.R
#
###########################################################

library(shiny)

rows <- c("A", "B", "C", "D", "E", "F", "G", "H")

shinyUI(fluidPage(
  includeCSS("www/style.css"),
  
  tags$head(
    tags$title('VIZN - Visual Interrogation of Zebrafish maNipulations')
  ),
  
  # Application title
  titlePanel(
    img(src="vizn.jpg", height = 126, width = 246),
    "VIZN - Visual Interrogation of Zebrafish maNipulations"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      # option to use sample data to demo the software  
      checkboxInput("sample", label="Demo with Sample Data", value = FALSE),
      
      # radio button to select 96 or 48 well plates
      #radioButtons("plateSelect", label="Select Plate Size", choices = list("96 wells" = 96, "48 wells" = 48), selected = 96),
      
      # upload Zebrabox spreadsheet
      fileInput('file', label=h4("Select Zebrabox Spreadsheet"), multiple=FALSE),
      
      conditionalPanel(
        condition="input.conditionedPanels == 'Activity Plot'",
            #response threshold setting
            numericInput("threshold", label="Response Threshold", value=2.5),
      
             # select what rows to average
             selectInput("row", label="Row Selection", choice = list("Row A" = 'A', "Row B" = 'B', "Row C" = 'C', "Row D" = 'D', "Row E" = 'E', "Row F" = 'F', "Row G" = 'G', "Row H" = 'H')),
             
             #adjust Plot_A axis
             sliderInput("xaxis", label=("Adjust Activity Plot Axis"), min=10, max=1950, value = c(1700,1950), animate=TRUE)
                      
      ),
      conditionalPanel(
        condition="input.conditionedPanels == 'Startle Responses'",
        #response threshold setting
        numericInput("threshold", label="Response Threshold", value=2.5),
        #grouping rows together options
        selectizeInput('g1', 'Group 1', choices = rows, multiple = TRUE),
        selectizeInput('g2', 'Group 2', choices = rows, multiple = TRUE),
        selectizeInput('g3', 'Group 3', choices = rows, multiple = TRUE),
        checkboxInput("group", label="Group Rows (check after selecting groups)", value = FALSE)
        
      )
      
      
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel("Instructions", uiOutput("intro")),
        
        #Plot A on the first tab
        tabPanel("Activity Plot", dataTableOutput(outputId="beginAndEnd"), plotOutput("plotAvgActinteg")),
        
        #Plots C and data summary tables on the second Tab
        tabPanel("Startle Responses", plotOutput("plotStarteResponses"), uiOutput("datasummary"), dataTableOutput(outputId="tableDataSummary"), uiOutput("ptitle"), dataTableOutput(outputId="tablePvalue")
        ),
        
        tabPanel("Results Table", downloadButton('downloadData', "Download Data"), dataTableOutput(outputId="ind_table")
        ),
        tabPanel("changelog", uiOutput("changelog")),
        id = "conditionedPanels"
      )
      
    )
    
  )
))
