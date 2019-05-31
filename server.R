
# This is the server script, it takes a file upload (myData) and performs automated runchart data wrangling using function "stable".
# It then plots the resulting dataframe into a chart and outputs the chart to the UI script. It also takes
# a number of user inputs (e.g. annotations) to customise the output. It closes with a download tool so that
# an image of the chart (named with the upload filename) can be downloaded.

shinyServer <- function(input, output) {
  
  myData <- reactive({
    inFile <- input$datafile
    if (is.null(inFile)) return(NULL)
    if (endsWith(inFile$name, '.xlsx')){
      data <- read_xlsx(inFile$datapath)
    } 
    else if (endsWith(inFile$name, '.xls')){
      data <- read_xls(inFile$datapath)
    } 
    else if (endsWith(inFile$name, '.csv')){
      data <- read_csv(inFile$datapath)%>%
        mutate_at(1, .funs = funs(dmy(.)))
    }
    
    ###WORK NEEDED: currently configured to Excel dates - needs work to accept different formats
    data <- data%>%
      mutate(date = as_date(as.POSIXct(as.numeric(date),origin = "1970-01-01", tz = "GMT")))%>%
      arrange(date)
      
  })
  
  #trigger <- reactive({
  #  trigger <- input$trigger
  #})
  
  #baseline <- reactive({
  #  baseline <- input$baseline
  #})
  
  shiftsens <- reactive({
    shiftsens <- input$shiftsens
    
  })
  
  heading <- reactive({
    heading <- input$heading
  })
  
  xaxis <- reactive({
    xaxis <- input$xaxis
  })
  
  yaxis <- reactive({
    yaxis <- input$yaxis
  })
  
  anno1 <- reactive({
    
    if(is.null(input$anno1))
    {return()}
    
    else {
    
    anno1 <- input$anno1}
  })
  
  event1 <- reactive({
    event1 <- input$event1
  })
  
  rundata <- reactive({
    
    if (is.null(myData()))
    {return()}
    
    else {
      
      RunChart(myData()[[2]], myData()[[1]], shiftsens())
      
    }
  })
  
  runplot <- reactive ({
    
    rundata <- rundata()
    
  ggplot(rundata) +
    geom_line(aes(x = subgroup, y=measure, group = 1), colour = "skyblue", size = 1) + 
    geom_point(aes(x = subgroup, y=measure, group = 1), colour = "skyblue", size = 3) +  
    geom_line(aes(x = subgroup, y=median, group = base_n), linetype = "longdash", colour = "orange") +
    geom_line(aes(x = subgroup, y=baselines, group = base_n), linetype = "solid", colour = "orange", size = 1) +
    geom_point(aes(x = subgroup, y=highlight, group = 1), colour = "red") +
    geom_text(aes(x = subgroup, y = median, label = base_label), vjust = 1, hjust = 0) +
    geom_point(aes(x = subgroup, y=as.numeric(trendind), group = 1), shape = 1, size = 5, colour = "blue") +
    theme(axis.text.x=element_text(angle = 90, hjust = 0), panel.background = element_rect(fill = "transparent")) +
    geom_vline(xintercept = event1(), linetype = "dashed")+
    geom_text(x = event1(), label = stringr::str_wrap(anno1(),30), y = max(as.numeric(rundata$measure))*0.1, vjust = 1)+
    scale_y_continuous(limits=c(0, max(as.numeric(rundata$measure))), expand = c(0, 0)) +
    #scale_x_continuous(breaks=pretty(subgroup, n=30)) +
    #scale_x_discrete(breaks = xbreaks) +
    xlab(xaxis()) + ylab(yaxis()) +
    ggtitle(heading())+
    theme_classic()+
    theme(plot.title = element_text(family = "TT arial", size = 14, face = "bold"),
                   axis.title.x = element_text(family = "TT arial", size = 11, face = "bold"),
                   axis.title.y = element_text(family = "TT arial", size = 11, face = "bold"))
  }) # ggplot chart
  
  output$runchart <- renderPlot({
    
    if (is.null(myData()))
    {return()}
    
    else {runplot()}
    
    })
  
  #output$rundata <- renderTable({rundata()})
  
  
  #Download handler produces image of chart named by filename of data upload.
  output$pullchart <- downloadHandler(
    filename = function() {
      paste0("Runchart of ", sub(pattern = "(.*)\\..*$", replacement = "\\1", input$datafile$name), ".png", sep = "")},
    content = function(file) {
      ggsave(file, plot = runplot(), device = "png")
    }
  )
  
}



## END