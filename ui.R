library(shiny)
library(ggvis)
shinyUI(fluidPage(
  
  # Application title
  titlePanel("SensiLabs Analytics"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
#       fileInput('file', 'Choose CSV File',
#                 accept=c('text/csv', 
#                          'text/comma-separated-values,text/plain', 
#                          '.csv')), 
      radioButtons("median", "Plot Type", c("All Signals","Median")), 
      radioButtons("subtractBSA", "Subtract BSA", c("Raw Signals", "BSA Subtracted")),
      actionButton("update", "Remove Selected Curves"),
      
      textInput("filename", "Enter filename"),
      downloadButton("downloadData", "Download Data"),
      textOutput("urlText"), 
      textOutput("queryText")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(align = "center",
              ggvisOutput("bindingCurves"),
              uiOutput("ui_output"),
              sliderInput("timeEval", "Statistics Evaluation Time Point", 1, 1200, 600, width = "90%"),
              tableOutput("statisticsTable"), 
              ggvisOutput("boxPlot"),
              uiOutput("ui_box")
    )
  )
))
