# Description - Describe what the app does (e.g. visualizes births data)
# User interface - how your app looks and elements users can interact with

shinyUI(fluidPage(
  
  title = "Runchart Builder",
  
  fluidRow(
    column(2, "Build your chart",
           fileInput("datafile", "Load your file here:",
                     accept = c('application/vnd.ms-excel',
                                '.xls',
                                '.xlsx',
                                'text/csv',
                                'text/comma-separated-values,text/plain',
                                '.csv')),
           #numericInput("baseline", "Choose number of points for first baseline", value = 12),
           #numericInput("trigger", "Choose number of points at which to rebase", value = 9),
           radioButtons("shiftsens", "Select sensitivity of rebase", choices = c("Any shift" = "newshiftpos", 
                                                                       "Sustained shift" = "newsusshiftpos",
                                                                       "None" = "none"),
                        selected = "none"),
           checkboxInput("percentage", "Using percentages?", value = FALSE)),
    
    
    # Show a plot of the generated distribution
    column(10,
           plotOutput("runchart"))
           #tableOutput("rundata"))
  ),
  
  
  fluidRow(column(3,textInput("anno1", "Annotation One")),
           column(3,textInput("heading", "Title for your chart", value =" ")),
           column(3,textInput("xaxis", "X Axis Title", value = " ")),
           column(3,textInput("yaxis", "Y Axis Title", value = " "))),
  fluidRow(column(3,dateInput("event1", "Date for Annotation")),
           column(3,downloadButton("pullchart", "Download chart")),
           column(3,actionButton("allGo", "Update inputs")))
           
  ))