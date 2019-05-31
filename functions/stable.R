# Function to produce Run Charts


# Function to produce rebased run charts
# Param measure is the measure
# Param subgroup is the row identifier (works with dates)
# Param shiftsens is set at either "newshiftpos" to interrupt at any shift, or "newsusshiftpos" to interrupt at sustained shifts only


RunChart = function(measure, subgroup, shiftsens, percentage) {
  
  dataDF = data.frame(measure,subgroup)%>%
    filter(!is.na(measure))%>%
    arrange(subgroup)#Create dataframe from vector data
  
  ###Remove missing values and reintroduce later - these neither make nor break shifts 
  if (any(is.na(measure))) {
    
    missingDF = data.frame(measure,subgroup)%>%
      filter(is.na(measure))%>%
      mutate(median = NA,
             baselines = NA,
             base_n = NA,
             base_label = NA,
             abovebelow = NA,
             #preserveTF = NA,
             abovebelowpreserved = NA,
             runNo = NA,
             runlength = NA,
             highlight = NA,
             trendind = NA) }
  
  #  if (length(subgroup) < 12)  {
  #    title = gsub( "Run ", "Line ", title)
  #    print(
  #      ggplot() +
  #        geom_line(aes(x=subgroup, y=measure, group = 1), data=dataDF) + 
  #        geom_point(aes(x=subgroup, y=measure, group = 1), data=dataDF) +  
  #        theme(axis.text.x=element_text(angle = 90, hjust = 0), panel.background = element_rect(fill = "transparent")) +
  #        scale_y_continuous(labels=scaler(), limits=c(max(min(dataDF$measure)*0.66,0), uplim)) +
  #        xlab(xlabel) + ylab(ylabel) +
  #ggtitle(title)+
  #ggtitle(bquote(atop(.(title), atop(italic(.("Median not plotted. Run charts should have at least 12 points.")), ""))) 
  #        ggtitle(expression(atop(title, atop(bold("Median not plotted. Run charts should have at least 12 points."), ""))))
  #    )
  #  } else {
  
  shiftpos = min(which(!is.na(dataDF$measure)))
  newshiftpos = shiftpos
  chrt = 0
  base_n = 0
  
  ### Calculate median for first [1:12]
  dataDF$median = round(median(dataDF$measure[shiftpos:(shiftpos+11)]),0)
  dataDF$baselines = as.numeric(NA) # Create baseline variable but don't calculate until checked for 12 points with no shift
  dataDF$base_n = base_n #Create grouping variable for baselines to disconnect lines
  dataDF$base_label = as.character(NA) #Create labelling variable for baselines
  
  i=1
  while (i <= length(dataDF$measure)) {
    
    ### Calculate shifts
    #Calculate points above or below median and number each run
    dataDF$abovebelow = 0
    dataDF$abovebelow[dataDF$median == 0] = -1   # If the median is zero treat as below
    if (percentage == TRUE) {dataDF$abovebelow[dataDF$median == 100] = 1} #If using percentages and the median is 100, treat as above
    dataDF$abovebelow[dataDF$measure < dataDF$median] = -1
    dataDF$abovebelow[dataDF$measure > dataDF$median] = 1
    
    # Overwrite 12 values at shiftpos with non-useful values if not interrupting baseline
    if(shiftsens == "none") {
      d <- length(dataDF$measure) - shiftpos
      
      if (d >= 11){
        dataDF$abovebelow[shiftpos:(shiftpos + 11)] <- 0}
      
      else {dataDF$abovebelow[shiftpos:length(dataDF$measure)] <- 0}
    }
    
    #coerce non-useful observations to NA (for fill function below)
    dataDF = dataDF%>%
      mutate(abovebelowpreserved = na_if(abovebelow, 0))
    
    #preserve first observation
    dataDF$abovebelowpreserved[1] <- dataDF$abovebelow[1]
    
    #fill non-useful observations with last useful observation
    #overwrite minimum/maximum values lining up along median as non-useful observations
    dataDF = dataDF%>%
      fill(abovebelowpreserved, .direction = "down")%>%
      mutate(abovebelowpreserved = ifelse((median == 0 & measure == 0) | (median == 100 & measure == 100), 0, abovebelowpreserved))
    
    # Overwrite 12 values at shiftpos with non-useful values if not interrupting baseline
    if(shiftsens == "none") {
      d <- length(dataDF$measure) - shiftpos
      
      if (d >= 11){
        dataDF$abovebelowpreserved[shiftpos:(shiftpos + 11)] <- 0}
      
      else {dataDF$abovebelowpreserved[shiftpos:length(dataDF$measure)] <- 0}
    }
    
    #Number the runs above or below the median   
    runChange = dataDF$abovebelowpreserved[-1L] != dataDF$abovebelowpreserved[-nrow(dataDF)] #List change points in data with TRUE
    runChange[dataDF$median[-1L] != dataDF$median[-nrow(dataDF)]] = TRUE #Start new run at rephase points
    dataDF$runNo = c(0,cumsum(runChange)) +1 #Number the runs
    
    # Create data frame with run lengths - excluding points on the center line
    runlengthDF = dataDF%>%
      filter(abovebelow!= 0)%>%
      dplyr::group_by(runNo)%>%
      dplyr::summarise(runlength = n()) 
    
    # Merge two dataframes
    dataDF = dataDF%>%
      left_join(runlengthDF, by = "runNo")%>%
      mutate(runlength = ifelse(abovebelowpreserved == 0, 0, runlength))%>%
      mutate(runlength = ifelse(median == 0 & measure == 0, 0, runlength))%>% #For cases with run of 0s on 0 median
      mutate(runlength = ifelse(median == 100 & measure == 100, 0, runlength)) #For cases with run of 100s on 100 median
    
    # For runs 6 or longer add data to highlights column
    dataDF$highlight = NA
    dataDF$highlight[dataDF$runlength >= 6 & dataDF$abovebelow != 0] = dataDF$measure[dataDF$runlength >= 6 & dataDF$abovebelow != 0]
    
    
    # Calculate the position of the next minimum 6 point shift (returns one if no more shifts)
    # Parameterise sensitivity of rebase to either interrupt with any shift or only sustained shifts, or don't interrupt
    if(shiftsens == "none") {
      newshiftpos = min(which.max(dataDF$runlength >= 9 & dataDF$runNo > dataDF$runNo[shiftpos]),nrow(dataDF))}
    if (shiftsens == "newshiftpos") {
      newshiftpos = min(which.max(dataDF$runlength >= 6 & dataDF$runNo > dataDF$runNo[shiftpos]),nrow(dataDF))}
    if (shiftsens == "newsusshiftpos") {
      newshiftpos = min(which.max(dataDF$runlength >= 9 & dataDF$runNo > dataDF$runNo[shiftpos]),nrow(dataDF))}
    
    #remove existing runlength column so as not to interfere with next join iteration
    dataDF = dataDF%>%
      select(-runlength)
    
    # Mark as new baseline if
    # (a) there are 12 data points (not counting blanks or NAs); and
    # (b) the 12 points are stable (i.e. no shifts starting within the 12 points)
    # (c) allow if 12 points start with a shift
    if (shiftpos+11 <= length(dataDF$measure) & (shiftpos+11 < newshiftpos | newshiftpos == 1))
    {dataDF$baselines[shiftpos:(shiftpos+11)] = dataDF$median[shiftpos:(shiftpos+11)]
    # Increment base_n to disconnect baselines
    base_n <- base_n + 1
    dataDF$base_n[shiftpos:length(dataDF$base_n)] = base_n
    # Label baseline
    dataDF$base_label[shiftpos] = as.character(round(dataDF$median[shiftpos]))
    }
    
    # If rebase occurs over 9-11 points at end of dataframe, rebase as temporary
    else if (shiftpos+11 >= length(dataDF$measure))
    {d <- length(dataDF$measure) - shiftpos
    # Increment base_n to disconnect baselines
    base_n <- base_n + 1
    dataDF$base_n[shiftpos:length(dataDF$base_n)] = base_n
    # Label baseline (Temporary)
    dataDF$base_label[shiftpos] = paste0("Temporary: ", round(dataDF$median[shiftpos]))
    break
    
    }
    
    ### Otherwise the current baseline is temporary and is labelled as such
    else  {
      #Increment baseline
      base_n <- base_n + 1
      dataDF$base_n[shiftpos:(newshiftpos - 1)] = base_n
      #Label baseline
      dataDF$base_label[shiftpos] = paste0("Temporary: ", round(dataDF$median[shiftpos]))
    }
    
    if (newshiftpos == 1) {
      
      break} #If newsusshiftpos is 1 there are no more sustained shifts and we can stop looping
    
    ### Recalculate median and extend to end
    dataDF$median[newshiftpos:length(dataDF$measure)] = round(median(dataDF$measure[newshiftpos:min(newshiftpos+11,(newshiftpos+dataDF$runlength[newshiftpos]-1), nrow(dataDF))]),0)  # Median
    dataDF$baselines[newshiftpos:length(dataDF$measure)] = NA
    
    shiftpos = newshiftpos #Make the next sustained shift position the start of the baseline in the next loop
    i = newshiftpos
    
    
  } # End of For loop
  
  ###Check for trend
  
  dataDF <- dataDF%>%
    dplyr::mutate(trender = case_when(
      row_number() == 1 ~ "none", 
      measure > lag(measure) ~ "up",
      measure < lag(measure) ~ "down",
      TRUE ~ "fill"))%>% #Assign trend categories
    mutate(trenderpres = na_if(trender, "fill"))%>% #Coerce values that stay the same to NA for fill function below
    fill(trenderpres, .direction = "down")%>% #Fill periods that stay the same with previous useful observation
    dplyr::mutate(trendNo = cumsum(trenderpres != lag(trenderpres) | row_number() == 1))%>% #Group trends by incrementing at changepoints
    group_by(trendNo)%>%
    dplyr::mutate(trendlength = sum(trender != "fill"))%>% #Calculate length of trends
    ungroup()%>%
    mutate(trendind = ifelse(trendlength >= 5, measure,#Flag reliable trends
                             ifelse(lead(trendlength)>= 5, measure, NA)))%>% #Very lazy but backfills at changepoints
    dplyr::select(-c(trender, trenderpres, trendNo, trendlength))#remove obsolete columns
  
  if (any(is.na(measure))) {
    
    dataDF <- bind_rows(dataDF, missingDF)
    
    dataDF <- dataDF%>%
      arrange(subgroup)}
  
  return(dataDF)
  
  #  } # End of 12 points If 
  
  
} # End of RunChart function


##Debug script
# 
# FVCAM <- read_excel("~/MHAIST/Core/Runcharts/Own attempt/Autochart/data/FVCAM.xlsx")
# debugonce(RunChart)
# RunChart(FVCAM$value, FVCAM$date, shiftsens = "none")
