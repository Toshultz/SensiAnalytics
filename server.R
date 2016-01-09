library(shiny)
library(data.table)
library(tidyr)
require(ggvis)

#define global sensors
dataSet = NULL
colorVec = c()


update_selection = function(data, location, session){
  
  print("click event")

  # dataSet[,data$sensors] <<- NULL
  colorVec[1,data$sensors] <<- "gray"
  
  sensorExists[1, data$sensors] <<- FALSE

  if(is.element(data$sensors, colnames(BSAsensors))){
    BSAsensors[,data$sensors] <<- NULL
  }
  else if(is.element(data$sensors, colnames(THCsensors))){
    THCsensors[,data$sensors] <<- NULL
  }
  else if(is.element(data$sensors, colnames(BIOTINsensors))){
    BIOTINsensors[,data$sensors] <<- NULL
  }
  
} 

#take the median value of all non NA entries
nonNAMedian <- function(x){
  
  nonZeroEntries = c();
  for(i in 1:length(x)){
    if(!is.na(x[i])){
      nonZeroEntries = c(nonZeroEntries, x[i])
    }
  }
  
  if(length(nonZeroEntries) == 0){med = NA}
  else(med = median(nonZeroEntries))
  return (med)
}

shinyServer(function(input, output, session) {
  
  # Return the components of the URL in a string:
  output$urlText <- renderText({
    paste(sep = "",
          "protocol: ", session$clientData$url_protocol, "\n",
          "hostname: ", session$clientData$url_hostname, "\n",
          "pathname: ", session$clientData$url_pathname, "\n",
          "port: ",     session$clientData$url_port,     "\n",
          "search: ",   session$clientData$url_search,   "\n"
    )
  })
  
  # Parse the GET query string

  output$queryText <- renderText({
    query <- parseQueryString(session$clientData$url_search)
    
    # Return a string with key-value pairs
    # paste(names(query), query, sep = "=", collapse=", ")
    # paste(names(query), query, sep = "=")
    
    #needed dirname when running from local machine, now try to move everything to AWS
    # dirname = "/Users/tylershultz/Desktop/BasicWebpage/"
    
    dirname = ""
    inFile <<- paste(dirname, names(query), sep = "")
    firstUpload <<- TRUE
    print(paste("in queryText, firstUpload = ", firstUpload))
    print(paste("in queryText, inFile = ", inFile))
  })
 
  
  uploadData <- function(dataSet, inFile){

    print(paste("in upload data, first upload = ", firstUpload))
    if(!is.null(dataSet) && firstUpload == FALSE){
      return(dataSet)
    }
    
    print("New upload:")
    print(firstUpload)
    if(firstUpload == TRUE){
      firstUpload <<- FALSE
      print("doing calcs...")
      print(inFile)
      userName <<- toString(read.csv(inFile, header = FALSE, nrows = 1)[1,1])
      ExperimentID <<- toString(read.csv(inFile, header = FALSE, nrows = 1)[1,2])
      dataSet = read.csv(inFile, header = TRUE, sep = ",", dec = ".", skip = 1)
      
      maxValue <<- max(dataSet$Value)

      numTimePoints = tail(dataSet$Iteration, 1)
      endPoint <<- numTimePoints - 1;
      

      timeMatrix = matrix(NA,numTimePoints,80)
      colnames(timeMatrix) <- c(0:79)
      

      sensorExists = matrix(FALSE, 1, 80)
      colnames(sensorExists) <- c(0:79)
      

      sensorppm = matrix(NA,numTimePoints,80)
      colnames(sensorppm) <- c(0:79)
      
      
      sensorType = matrix("EMPTY", 1, 80)
      colnames(sensorType) <- c(0:79)
      
      #populate sensorExists, sensorppm, timeMatrix, sensorType
      firstPass = TRUE
      for(i in 1:length(dataSet$Sensor)){
        sensor = toString(dataSet$Sensor[i])
        iteration = dataSet$Iteration[i]
        if(firstPass){
          if(sensorExists[1,sensor] == TRUE){
            firstPass = FALSE
          }
          sensorType[1,sensor] = toString(dataSet$Type[i])
          sensorExists[1,sensor] = TRUE
        }
        
        timeMatrix[iteration,sensor] = dataSet$Time[i]
        sensorppm[iteration,sensor] = dataSet$Value[i]
      }
      #################################

      sensorExists <<- sensorExists
      
      timeVec <<- timeMatrix[,1] #save just the time stamp for the first sensor
#       updateSliderInput(session, "timeEval", label = "Statistics Evaluation Time Point", 
#                         min = 1, max = max(timeVec))
      
      ####################
      #find median time point, create data frame with median time point and all existing sensors
      #also create a color vector that matches the non-empty entries of sensorType
      timeMedian = head(apply(timeMatrix, 1, nonNAMedian), -1)
      df = data.frame(timeMedian)
      
      
      colorVec = c()
      columnNames = c()
      index = 0
      for(i in 0:79){
        sensor = toString(i)
        if(sensorExists[1,sensor]){
          type = sensorType[1,sensor]
          color = "black"
          if(type == "BSA"){
            index = index + 1
            colorVec[index] = "yellow"
            columnNames[index] = i
          }
          else if(type == "THC"){
            index = index + 1
            colorVec[index] = "green"
            columnNames[index] = i
            
          }
          else if(type == "BIOTIN"){
            index = index + 1
            colorVec[index] = "blue"
            columnNames[index] = i
            
          }
          y = sensorppm[,sensor]
          y = head(y,-1)
          y = data.frame(y)
          colnames(y) = c(sensor)
          
          df = cbind(df, y)
          
        }
      }
      
      colorVec <- matrix(colorVec, 1, length(colorVec))
      # colorVec <- t(data.frame(colorVec))
      colnames(colorVec) <- columnNames
      colorVec <<- colorVec
      
      
      #sort sensors into their types for quicker access when calculating statistics
      
      BSAsensors <- matrix(0,numTimePoints,0)
      THCsensors <- matrix(0,numTimePoints,0)
      BIOTINsensors <- matrix(0,numTimePoints,0)
      
      BSAnames = c()
      THCnames = c()
      BIOTINnames = c()
  
      for(i in 1:80){
        if(sensorType[i] == "BSA"){
          BSAsensors <- cbind(BSAsensors, sensorppm[,i])
          BSAnames <- rbind(BSAnames, i-1)
        }else if(sensorType[i] == "THC"){
          THCsensors <- cbind(THCsensors, sensorppm[,i])
          THCnames <- rbind(THCnames, i-1)
        }else if(sensorType[i] == "BIOTIN"){
          BIOTINsensors <- cbind(BIOTINsensors, sensorppm[,i])
          BIOTINnames <- rbind(BIOTINnames, i-1)
        }
      }


      BSAsensors <- data.frame(BSAsensors)
      THCsensors <- data.frame(THCsensors)
      BIOTINsensors <- data.frame(BIOTINsensors)
      
      colnames(BSAsensors) = BSAnames
      colnames(THCsensors) = THCnames
      colnames(BIOTINsensors) = BIOTINnames
      
      BSAsensors <<- BSAsensors
      THCsensors <<- THCsensors
      BIOTINsensors <<- BIOTINsensors
      
      ##########################
      
      return(df)
    }
      
    
  }
  
  output$statisticsTable <- renderTable({
    
    if(input$update){}
    timeEval = input$timeEval
    
    # dataSet <<- uploadData(dataSet)
    if(!is.null(dataSet)){
      dataPoint = which.min(abs(timeVec - timeEval))
      
      BSAend = as.numeric(BSAsensors[dataPoint,])
      THCend = as.numeric(THCsensors[dataPoint,])
      BIOTINend = as.numeric(BIOTINsensors[dataPoint,])
      
      BSAmedian = round(median(BSAend),2)
      BSAmean = round(mean(BSAend),2)
      
      THCmedian = round(median(THCend),2)
      THCmean = round(mean(THCend),2)
      
      BIOTINmedian = round(median(BIOTINend),2)
      BIOTINmean = round(mean(BIOTINend),2)
      
      BSA_STDEV = round(sd(BSAend),2)
      BSA_CV = round(BSA_STDEV/BSAmean,2)
      
      THC_STDEV = round(sd(THCend),2)
      THC_CV = round(THC_STDEV/THCmean,2)
      
      BIOTIN_STDEV = round(sd(BIOTINend),2)
      BIOTIN_CV = round(BIOTIN_STDEV/BIOTINmean,2)
      
      BSA = c(BSAmean, BSAmedian, BSA_STDEV, BSA_CV)
      THC = c(THCmean, THCmedian, THC_STDEV, THC_CV)
      BIOTIN = c(BIOTINmean, BIOTINmedian, BIOTIN_STDEV, BIOTIN_CV)
      
      summaryTable = data.table(cbind(BSA, THC, BIOTIN))
      row.names(summaryTable) = c("Mean", "Median", "SD", "CV")
      summaryTable <<- summaryTable
      summaryTable
    }
  
  })
  output$ui_output <- reactive({
    
    if(input$update){}


    dataSet <<- uploadData(dataSet, inFile)
  
    if(!is.null(dataSet)){
      
      if(input$median == "Median"){
        
        BSA_median = head(apply(BSAsensors, 1,nonNAMedian), -1)
        THC_median = head(apply(THCsensors, 1,nonNAMedian), -1)
        BIOTIN_median = head(apply(BIOTINsensors, 1,nonNAMedian), -1)
        
        if(input$subtractBSA == "BSA Subtracted"){
          THC_median = THC_median - BSA_median
          BIOTIN_median = BIOTIN_median - BSA_median
          BSA_median = BSA_median - BSA_median
        }
        
        timeMed = dataSet$timeMedian
        medData = data.frame(timeMed, BSA_median, THC_median, BIOTIN_median)
        
        gather(medData, sensors, ppm, -(timeMed)) -> medianDF
        
        timeEval = input$timeEval
        x_ln = c(timeEval,timeEval)
        y_ln = c(-20, maxValue+10)
        linedat <- data.frame(x_ln, y_ln)
        
        ggvis(medianDF, ~timeMed, ~ppm, stroke = ~sensors) %>%
          layer_lines(strokeWidth := 2)%>%
          layer_model_predictions(model = "lm", stroke := NA)%>%
          layer_paths(x = ~x_ln, y = ~y_ln, stroke := "black", strokeWidth := 2, data = linedat)%>%
          add_axis(type = "x", title = "Time (s)")%>%
          add_axis(type = "x", orient = "top", title = paste(userName, "ExpID:", ExperimentID, sep=" "), 
                   properties = axis_props(
                     axis = list(stroke = "white"),
                     ticks = list(stroke = "white"),
                     title = list(fontSize = 15),
                     labels = list(fontSize = 0)))%>%
          add_axis(type = "y", title = "ppm")%>%
          scale_nominal("stroke", range = c("yellow", "green", "blue"))%>%
          bind_shiny("bindingCurves", "ui_output")
        
      }else{
      
        if(input$subtractBSA == "BSA Subtracted"){
          BSA_median = head(apply(BSAsensors, 1,nonNAMedian), -1)
          dataSubtracted = data.frame(apply(dataSet, 2, function(x) x-BSA_median))
          dataSubtracted$timeMedian = dataSet$timeMedian
          colnames(dataSubtracted) = colnames(dataSet)
          gather(dataSubtracted, sensors, ppm, -(timeMedian)) -> allSensorsDF

        }else{
          gather(dataSet, sensors, ppm, -(timeMedian)) -> allSensorsDF
          
        }
        
        timeEval = input$timeEval
        x_ln = c(timeEval,timeEval)
        y_ln = c(-20, maxValue+10)
        linedat <- data.frame(x_ln, y_ln)
        
        ggvis(allSensorsDF, ~timeMedian, ~ppm, stroke = ~sensors) %>%
          layer_lines(strokeWidth.hover := 4, strokeWidth := 2)%>%
          layer_model_predictions(model = "lm", stroke := NA)%>%
          layer_paths(x = ~x_ln, y = ~y_ln, stroke := "black", strokeWidth := 2, data = linedat)%>%
          scale_nominal("stroke", range = as.vector(colorVec))%>%
          hide_legend("stroke")%>%
          add_axis(type = "x", title = "Time (s)")%>%
          add_axis(type = "x", orient = "top", title = paste(userName, "ExpID:", ExperimentID), 
                   properties = axis_props(
                   axis = list(stroke = "white"),
                   ticks = list(stroke = "white"),
                   title = list(fontSize = 15),
                   labels = list(fontSize = 0)))%>%
          add_axis(type = "y", title = "ppm")%>%
          add_tooltip(function(allSensorsDF) allSensorsDF$sensors) %>%
          handle_click(update_selection) %>%
          bind_shiny("bindingCurves", "ui_output")
      }
    }
    
  })
  
  output$ui_box <- reactive({
    
    dataSet <<- uploadData(dataSet)
    
    if(input$update){}
    
    if(!is.null(dataSet)){
      
      timeEval = input$timeEval
      
      dataPoint = which.min(abs(timeVec - timeEval))
      
      timeMin = floor(timeEval/60)
      timeSec = timeEval%%60
      plotTitle = paste("ExpID:", ExperimentID, ".", "Statistics at", timeMin, "min", timeSec, "sec")
      
      BSA = as.numeric(BSAsensors[dataPoint,])
      THC = as.numeric(THCsensors[dataPoint,])
      BIOTIN = as.numeric(BIOTINsensors[dataPoint,])
      
      boxData = data.frame(cbind(BSA, THC, BIOTIN))
      
      gather(boxData, sensor, ppm) -> boxDF
      
      ggvis(boxDF, ~factor(sensor), ~ppm)%>%
        layer_boxplots(width = 1)%>%
        add_axis(type = "x", title = "Sensor Type")%>%
        add_axis(type = "x", orient = "top", title = plotTitle,
                 properties = axis_props(
                 axis = list(stroke = "white"),
                 ticks = list(stroke = "white"),
                 title = list(fontSize = 15),
                 labels = list(fontSize = 0)))%>%
        bind_shiny("boxPlot", "ui_box")
    }
    
  })

  output$downloadData <- downloadHandler(
    filename = function(){paste(input$filename, '.csv',sep='')},
    content = function(file){write.csv(summaryTable, file)}
  )
  
})