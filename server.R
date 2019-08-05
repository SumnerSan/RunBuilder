
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
      arrange(date)%>%
      mutate(value = as.numeric(value))
      
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
  
  heading <- eventReactive(input$allGo,{
    heading <- input$heading
  }, ignoreNULL = FALSE)
  
  xaxis <- eventReactive(input$allGo,{
    xaxis <- input$xaxis
  }, ignoreNULL = FALSE)
  
  yaxis <- eventReactive(input$allGo,{
    yaxis <- input$yaxis
  }, ignoreNULL = FALSE)
  
  percentage <- reactive({
    percentage <- input$percentage
  })
  
  anno1 <- eventReactive(input$allGo,{
    
    # if(is.null(input$anno1))
    # {return()}
    # 
    # else {
    
    anno1 <- input$anno1
    #}
  }, ignoreNULL = FALSE)
  
  event1 <- reactive({
    event1 <- input$event1
  })
  
  rundata <- reactive({
    
    if (is.null(myData()))
    {return()}
    
    else {
      
      RunChart(myData()[[2]], myData()[[1]], shiftsens(), percentage())
      
    }
  })
  
  runplot <- reactive ({
    
    rundata <- rundata()
  
  ggplot(rundata, aes(x = subgroup)) +
    geom_line(aes(y=measure, group = 1), colour = "#00a2e5", size = 1) + 
    geom_point(aes(y=measure, group = 1), colour = "#00a2e5", size = 2) +  
    geom_line(aes(y=median, group = base_n), linetype = "longdash", colour = "#ffcd04") +
    geom_line(aes(y=baselines, group = base_n), linetype = "solid", colour = "#ffcd04", size = 1) +
    geom_point(aes(y=highlight, group = 1), colour = "#ffcd04", size = 2) +
    geom_text(aes(y = median, label = base_label), vjust = 1, hjust = 0) +
    geom_point(aes(y=as.numeric(trendind), group = 1), shape = 1, size = 5, colour = "#007db3") +
    theme(axis.text.x=element_text(angle = 90, hjust = 0), panel.background = element_rect(fill = "transparent")) +
    geom_vline(xintercept = event1(), linetype = "dashed")+
    geom_text(x = event1(), label = stringr::str_wrap(anno1(),30), y = max(as.numeric(rundata$measure))*0.1, vjust = 1)+
    scale_y_continuous(limits=c(0, max(as.numeric(rundata$measure))+0.1*max(as.numeric(rundata$measure))), expand = c(0, 0)) +
    #scale_x_continuous(breaks=pretty(subgroup, n=30)) +
    #scale_x_discrete(breaks = xbreaks) +
    xlab(xaxis()) + ylab(yaxis()) +
    scale_x_date(breaks = "3 days", date_labels = "%b")+ #NEEDED - dynamically change based on data range
    ggtitle(heading())+
    theme_classic()+
    theme(plot.title = element_text(size = 14, face = "bold"),
          axis.title.x = element_text(size = 11, face = "bold"),
          #axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.y = element_text(size = 11, face = "bold"))
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
