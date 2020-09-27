library(shiny)
library(rhandsontable)
library(ggplot2)
library(scales)
library(stats)
library(deming)
library(MASS)

shinyServer (function(input,output,session)({
  
  addResourcePath(prefix="Data",directoryPath="Data")
  
  
#################################function Weigthed Least Square Linear
  
WlsqLinear <- function(x,y,wt) {
    
    invSigma2 <- 1/wt^2
    x2InvSigma2 <- (x^2)/(wt^2)
    xInvSigma2 <- x/wt^2
    yInvSigma2 <- y/wt^2
    xyInvSigma2 <- x*y/wt^2
    
    df <- data.frame(invSigma2,x2InvSigma2, xInvSigma2, yInvSigma2, xyInvSigma2)
    lsqSums <- colSums(df, na.rm = TRUE, dims = 1)
    
    Delta <- lsqSums[1]*lsqSums[2]-lsqSums[3]^2
    a <- (lsqSums[2]*lsqSums[4]-lsqSums[3]*lsqSums[5])/Delta
    b <- (lsqSums[1]*lsqSums[5]-lsqSums[3]*lsqSums[4])/Delta
    ua <- sqrt(lsqSums[2]/Delta)
    ub <- sqrt(lsqSums[1]/Delta)
    
    values <- c(a,b)
    uncertainties <- c(ua,ub)
    Results <- data.frame(values, uncertainties)
    row.names(Results) <- c("intercept", "slope")
    
    return(Results)
    
  }

#################################Simple scatter plot and linear regression
####LogPlot control
output$loglogPlot <- renderUI({
  
  if (input$loglog == TRUE) {
    tagList(
      hr(),
      h3("Log-Log Graph"),
      plotOutput("plotAutoLog", width = 650)
    )		
  }#closeif
})#end

output$loglogControl <- renderUI({
  
  if (input$loglog == TRUE) {
    tagList(
      h4("Log-Log Graph"),
      fluidRow(
        column(
          textInput("xTitleL", h5("x title"), value = "x", width = 150),
          numericInput("xMinL", h5("min x"), value = 0.1, min = 0.0000001, width = 100),
          numericInput("xMaxL", h5("max x"), value = 100, min = 0.0000001, width = 100),
          checkboxInput("showLineLog", "show fitted straight line", value = FALSE),
          width = 6),
        column(
          textInput("yTitleL", h5("y title"), value = "y", width = 150),
          numericInput("yMinL", h5("min y"), value = 0.1, width = 100),
          numericInput("yMaxL", h5("max y"), value = 100, width = 100),
          width = 6)
      )#endfluidRow
    )#endTagList
  }#endif
  
})

#####

  numLines <- 200
  X <- numeric(length = numLines)*NA
  DX <- numeric(length = numLines)*NA
  Y <- numeric(length = numLines)*NA
  DY <- numeric(length = numLines)*NA
  DF <- data.frame(X, DX, Y, DY)
  
  
  values <- reactiveValues(data=as.data.frame(DF))
 
  observe({
    if(!is.null(input$table))
      values$data <- hot_to_r(input$table)
  })
  
  output$table <- renderRHandsontable({
    decPlaces <- switch(trunc(input$numDecPlaces),"0.0","0.00","0.000","0.0000","0.00000")
    rhandsontable(values$data, height = 500) %>% 
      hot_cols("float",format = decPlaces, width = 70, language = "en-US")

  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste("Data.csv", sep='') },
    content = function(file) {
      numRow <- max(apply(values$data,2,function (x) length(na.omit(x))))
      exportTable <- values$data[1:numRow,]
      write.csv(exportTable, file, row.names = FALSE, na = "")
    })
    
  
  output$plotAuto <- renderPlot({
    
    xTitle <- ""
    try({xTitleTemp <- paste("expression(",input$xTitle,")", sep = "")
    xTitle <- eval(parse(text=xTitleTemp))
    }, silent = TRUE)
    yTitle <- ""
    try({yTitleTemp <- paste("expression(",input$yTitle,")", sep = "")
    yTitle <- eval(parse(text=yTitleTemp))
    }, silent = TRUE)
    
    plotAuto <- ggplot() +
      scale_x_continuous(xTitle, limits = c(input$xMin,input$xMax))+
      scale_y_continuous(yTitle, limits = c(input$yMin,input$yMax))+
      theme_bw()
    plotAuto <- plotAuto + theme(axis.title = element_text(size = 24), axis.text = element_text(size = 18), 
                                 plot.title = element_text(size = 24), legend.text = element_text(size = 12), legend.box = 
                                   "vertical",
                                 legend.title = element_text(size = 16))
    plotAuto <- plotAuto + geom_point(data = values$data, aes(X, Y), colour = 1) +
      geom_errorbarh(data = values$data, aes(x=X, y=Y, xmin = X - DX, xmax = X + DX),colour = 1, height = 0) +
      geom_errorbar(data = values$data, aes(x=X, ymin= Y - DY, ymax = Y + DY),colour = 1, width = 0) 
    
    try( if (input$fitting == TRUE) {
      
      if (!is.na(values$data$DX) && !is.na(values$data$DY)) {
        linearFit <- deming(Y ~ X, data = values$data, xstd = DX, ystd = DY)
        plotAuto <- plotAuto + geom_abline(intercept = linearFit$coefficients[1], slope = linearFit$coefficients[2], colour = 2)
      } else if (!is.na(values$data$DY)) {
        weightingFactor <- values$data$DY
        linearFit <- WlsqLinear(x = values$data$X, y = values$data$Y, wt = weightingFactor) 
        plotAuto <- plotAuto + geom_abline(intercept = linearFit[1,1], slope = linearFit[2,1], colour = 2)
      } else {
        weightingFactor <- NULL
        linearFit <- lm(Y ~ X, data = values$data, weights = weightingFactor)
        n <- summary(linearFit)
      plotAuto <- plotAuto + geom_abline(intercept = n$coefficients[1,1], slope = n$coefficients[2,1], colour = 2)
      }
    }
    )#end try
    
    return(plotAuto)
    
  }) #end Plot Auto
  
  output$fitTable <- renderTable({
    
    
    if (input$fitting == TRUE) {
      
      if (!is.na(values$data$DX) && !is.na(values$data$DY)) {
        linearFit <- deming(Y ~ X, data = values$data, xstd = DX, ystd = DY)
        bb <- scientific(linearFit$coefficients[1],3)
        Dbb <- scientific(linearFit$se[1],4)
        aa <- scientific(linearFit$coefficients[2],3)
        Daa <- scientific(linearFit$se[2],4)
        coefficient <- c(paste("a",intToUtf8(0x2081L)), paste(intToUtf8(0x0394L),"a",intToUtf8(0x2081L)), paste("b",intToUtf8(0x2081L)), paste(intToUtf8(0x0394L),"b",intToUtf8(0x2081L)))
      } else if (!is.na(values$data$DY)) {
        weightingFactor <- values$data$DY
        linearFit <- WlsqLinear(x = values$data$X, y = values$data$Y, wt = weightingFactor) 
        bb <- scientific(linearFit[1,1],3)
        Dbb <- scientific(linearFit[1,2],4)
        aa <- scientific(linearFit[2,1],3)
        Daa <- scientific(linearFit[2,2],4)
        coefficient <- c(paste("a",intToUtf8(0x2082L)), paste(intToUtf8(0x0394L),"a",intToUtf8(0x2082L)), paste("b",intToUtf8(0x2082L)), paste(intToUtf8(0x0394L),"b",intToUtf8(0x2082L)))
      } else {
        weightingFactor <- NULL
        linearFit <- lm(Y ~ X, data = values$data, weights = weightingFactor)
        n <- summary(linearFit)
        n <- summary(linearFit)
        bb <- scientific(n$coefficients[1,1],3)
        Dbb <- scientific(n$coefficients[1,2],4)
        aa <- scientific(n$coefficients[2,1],3)
        Daa <- scientific(n$coefficients[2,2],4)
        coefficient <- c(paste("a",intToUtf8(0x2083L)), paste(intToUtf8(0x0394L),"a",intToUtf8(0x2083L)), paste("b",intToUtf8(0x2083L)), paste(intToUtf8(0x0394L),"b",intToUtf8(0x2083L)))
      }
      
      #coefficient <- c("a", paste(intToUtf8(0x0394L),"a"), "b", paste(intToUtf8(0x0394L),"b"))
      value <- c(aa, Daa, bb, Dbb)
      fitTable <- data.frame(coefficient, value)
      colnames(fitTable) <- c("", "")
    } else fitTable <- NULL
    return(fitTable)
    
  }) #end fitTable
  
# ####Log-Log graph
#
   output$plotAutoLog <- renderPlot({

    xTitle <- ""
    try({xTitleTemp <- paste("expression(",input$xTitleL,")", sep = "")
    xTitle <- eval(parse(text=xTitleTemp))
    }, silent = TRUE)
    yTitle <- ""
    try({yTitleTemp <- paste("expression(",input$yTitleL,")", sep = "")
    yTitle <- eval(parse(text=yTitleTemp))
    }, silent = TRUE)
   
    breaks <- 10^(-10:10)
    minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9)) 
    
    plotAutoLog <- ggplot() +

       scale_x_log10(xTitle, limits = c(input$xMinL,input$xMaxL), breaks = breaks, minor_breaks = minor_breaks,
                     labels = trans_format("log10", math_format(10^.x))) +

       scale_y_log10(yTitle, limits = c(input$yMinL, input$yMaxL), breaks = breaks, minor_breaks = minor_breaks,
                     labels = trans_format("log10", math_format(10^.x))) +
       theme_bw()
     
     plotAutoLog <- plotAutoLog + theme(axis.title = element_text(size = 24), axis.text = element_text(size = 18),
                   plot.title = element_text(size = 32), legend.text = element_text(size = 12), legend.box = "vertical",
                   legend.title = element_text(size = 16))
     plotAutoLog <- plotAutoLog + annotation_logticks(sides = "trbl")

     
     #Add curves

     plotAutoLog <- plotAutoLog + geom_point(data = values$data, aes(X, Y), colour = 1)
     
     
     if (input$showLineLog == TRUE) plotAutoLog <- plotAutoLog + geom_smooth(data = ggplot_build(plotAutoLog)$data[[2]], mapping = aes(x = 10^x, y = 10^y), method = "lm", se = FALSE, colour = 2)

    return(plotAutoLog)

  })

#################################Simple histogram plotter and with mean and standard deviation calculator
  
  output$select <- renderUI({
    selectInput("showHist", h5("Show histogram for column:"), choices = c(1:input$numCol),width = 150)
  })
  numberOfLines <- 200
  numberOfCols <- 30#input$numCol
  A <- numeric(length = numberOfLines) * NA
  colTitles <- quote("Col 1")
  dfHist <- data.frame(A)
  if (numberOfCols > 1) {
    for (ii in 1:(numberOfCols-1)) {
      
      B <- numeric(length = numberOfLines) * NA
      dfHist <- data.frame(dfHist,B)
      colTitles <- c(colTitles,paste("Col",ii+1))
    }#endfor
  }#end if
  
  names(dfHist) <- colTitles
  
  valuesHist <- reactiveValues(data=as.data.frame(dfHist))
  
  observe({
    if(!is.null(input$tableHist))
      valuesHist$data <- hot_to_r(input$tableHist)
  })
  

  output$tableHist <- renderRHandsontable({
    decPlacesHist <- switch(trunc(input$numDecPlacesHist),"0.0","0.00","0.000","0.0000","0.00000")
    rhandsontable(valuesHist$data, height = 500) %>% hot_cols("float",format = decPlacesHist, width = 70, language = "en-US")
  })
  
  output$downloadDataHist <- downloadHandler(
    filename = function() { paste("DataHist.csv", sep='') },
    content = function(file) {
      exportTable <- valuesHist$data[,1:input$numCol]
      if(input$numCol > 1) {
        numRow <- max(apply(exportTable,2,function (x) length(na.omit(x))))
        exportTable2 <- exportTable[1:numRow,]
      } else {numRow <- length(na.omit(exportTable))
        exportTable2 <- exportTable[1:numRow]
      }

     write.csv(exportTable2, file, row.names = FALSE, na = "")
  })
  
  output$statTable <- renderTable({
    
    numberOfCols <- input$numCol
    
    calcTable <- valuesHist$data
    
    
    meanValues<-NULL
    sdValues<-NULL
    colTitles <- NULL

      for (ii in 1:(numberOfCols)) {

        meanCol <- mean(calcTable[,ii], na.rm = TRUE)
        meanValues <- c(meanValues, scientific(meanCol ,3))
        sdValues <- c(sdValues, scientific( sd(calcTable[,ii], na.rm = TRUE),3))
        colTitles <- c(colTitles,paste("Col",ii))
        statTable <- data.frame(colTitles,meanValues,sdValues)
        names(statTable) <- c("data", "mean", "standard deviation")
        
        
      }#endfor
    write.csv(statTable, file = "Data/DataRes.csv", row.names = FALSE)
    return(statTable)
    
  }) #end statTable
  
  output$plotHist <- renderPlot({
    
    
    calcTable <- valuesHist$data
    
    colNumber <- as.numeric(input$showHist)
    
    
    
    if (!is.na(calcTable[,colNumber])) {
      
      meanGauss <- mean(calcTable[,colNumber], na.rm = TRUE)
      sdGauss <- sd(calcTable[,colNumber], na.rm = TRUE)
      nGauss <- length(na.omit(calcTable[,colNumber]))
      
    
    xTitle <- ""
    try({xTitleTemp <- paste("expression(",input$xTitleH,")", sep = "")
    xTitle <- eval(parse(text=xTitleTemp))
    }, silent = TRUE)
    yTitle <- ""
    try({yTitleTemp <- paste("expression(",input$yTitleH,")", sep = "")
    yTitle <- eval(parse(text=yTitleTemp))
    }, silent = TRUE)
    plotHist <- ggplot(data = calcTable, aes(calcTable[,colNumber])) +
      scale_x_continuous(xTitle, limits = c(input$xMinH,input$xMaxH))+
      scale_y_continuous(yTitle, limits = c(input$yMinH,input$yMaxH))+
      theme_bw()
    plotHist <- plotHist + theme(axis.title = element_text(size = 24), axis.text = element_text(size = 18), 
                                 plot.title = element_text(size = 24), legend.text = element_text(size = 12), legend.box = 
                                   "vertical",
                                 legend.title = element_text(size = 16))
    plotHist <- plotHist + 
      stat_bin(binwidth = input$binSize, boundary = input$dataBound, closed = "left", colour = "black", fill = "light grey", aes(y = ..count..), pad = TRUE)    
      
    
    if (input$normal == TRUE) plotHist <- plotHist + 
      stat_function( 
        fun = function(x, mean, sd, n, bw){ 
          dnorm(x = x, mean = mean, sd = sd) * n * bw
        }, 
        args = c(mean = meanGauss, sd = sdGauss, n = nGauss, bw = input$binSize),
        colour = "red")
    
    } else plotHist <- NULL
    
    return(plotHist)
    
  }) #end PlotHist
  
    
#end Server  
}) 
)