############################################################
#
#   VIZN - Visual Interrogation of Zebrafish MaNipulatons
#
#     Created by Anthony Scott
#       May 28th, 2015
#
#     server.R
#
###########################################################

library(shiny)
source("visioncounts.R")


#allow for a 35Mb raw data spreadsheet to be uploaded, this can be increased as needed
options(shiny.maxRequestSize = 40*1024^2)

shinyServer(function(input, output) {
  
  
  
  output$plotAvgActinteg <- renderPlot({
    # assign variables from the UI inputs
    inFile <- input$file
    inRow <- input$row
    inData <- input$data
    threshold <- input$threshold
    inSample <- input$sample
    
    # creates a progress bar to show the user its doing something in the background
    withProgress(message = "Rendering Plot... Please Wait", value = 0, {
      
      # make sure uploaded file is not NULL; prevents errors, but only if Sample Data is not being used
      if (inSample == FALSE){
        if (is.null(inFile))
          return(NULL)
      }
      
      ## calculate and plot NORMALIZING to the overall average across all - This is the DEFAULT setting
      
      ## check if using Sample Data or User supplied data 
      # use data is being used
      if (inSample == FALSE){
        rawdata <- read.delim(inFile$datapath, sep="\t", header = TRUE)
      }
      
      #if sample data is being used
      if (inSample == TRUE){
        rawdata <- read.delim("www/sampleData.XLS", sep="\t", header = TRUE)
      }
      # increment progress counter
      incProgress(amount = 1/4)
      
      # this line of code exists to fix a bug in the viewpoint software which causes randomy wells to be "measured" after
      # the recording stop time 
      rawdata <- subset(rawdata, rawdata$end <= 1950)
      
      incProgress(amount = 2/4)
      
      # group the aniamls based on user selected Row letter
      animal <- subset(rawdata, grepl(inRow, rawdata$animal))
      
      # group the individual fish by start time and average their actinteg values
      Activity_AVG <- aggregate(animal$actinteg ~ animal$start, FUN=mean)
      
      #ignores the first 10 seconds in normalization
      Activity_AVG <- subset(Activity_AVG, Activity_AVG$`animal$start` > 10)
      
      incProgress(amount = 3/4)
      
      timepoints <- Activity_AVG[,2]
      norm_avg <- mean(timepoints)
      
      x <- Activity_AVG[,1]
      y <- Activity_AVG[,2]/norm_avg
      incProgress(amount = 1)
      
      plot(x,y, type="l", xlab="Seconds", ylab ="Actinteg Averages", xlim=(input$xaxis), main = "Activity Levels", ylim=c(0,8))
      
      # draw  a line across the graph indicating the user set threshold
      abline(a=threshold, b=0, h=threshold)
    })   
    
    
    
  })
  
  
  ## Create Plots based on Row-selection-dropbox for average of row of fish
  output$plotStarteResponses <- renderPlot({
    # assign variables from the UI inputs
    inFile <- input$file
    inRow <- input$row
    inData <- input$data
    threshold <- input$threshold
    inGroup <- input$group
    inSample <- input$sample
#    inPlateNum <- input$plateSelect
    
    # make sure uploaded file is not NULL, prevents errors, only if using user data
    if (inSample == FALSE){
      if (is.null(inFile))
        return(NULL)
    }
    
    
    # read in raw data and append well numbers, doing this here instead of in visioncounts_perwell function theoretically decreases run-time
    # check if using Sample Data or User supplied data    
    if (inSample == FALSE){
      rawdata <- read.delim(inFile$datapath, sep="\t", header = TRUE)
    }
    if (inSample == TRUE){
      rawdata <- read.delim("www/sampleData.XLS", sep="\t", header = TRUE)
    }
    
    # determine if this rawdata is from a 96 well plate or a 48 well plate
    if (rawdata[96,1] == 'w096'){
      inPlateNum <- 96
    } else if (rawdata[96,1] == 'w048'){
      inPlateNum <- 48
    } else {
      inPlateNum <- 0
    }
    
    validate(
      need(inPlateNum == 96 || inPlateNum == 48, "I cannot determine if your plate has 96 or 48 wells. This is likely because your spreadsheet has an incorrect number of rows. There is nothing else I can do. Sorry!")
      )
    
    # this line of code exists to fix a bug in the viewpoint software which causes randomy wells to be "measured" after
    # the recording stop time 
    rawdata <- subset(rawdata, rawdata$end <= 1950)
    rowNumSuccess<-FALSE
    
    if (inPlateNum == 96){
      rawdata$wellnum <- 1:96
    } else if (inPlateNum == 48 ){
      ## TODO Catch error here if incorrect number of rows and display error message
      tryCatch({
        rawdata$wellnum <- 1:48
        rowNumSuccess <- TRUE
        
      }, error = validate(
        need(rowNumSuccess, "Your plate has an incorrect number of rows. There is nothing more I can do. Sorry!" ))
      )
    }
    
    # needed variables for time, originally were input by user, but hard-coding these creates a cleaner UI
    time1 <- 1800
    time2 <- 1830
    time3 <- 1860
    time4 <- 1890
    time5 <- 1920
    
    if (inPlateNum == 96){
      withProgress(message = "Rendering... Please Wait", value = 0, {   
        
        # make a loop to cycle through all 96 wells and assign it a variable based on Row (example: well 1 = A1, well 96 = H12)       
        for (i in 1:12)
        {
          
          assign(paste("A",i, sep=""), visioncounts_perwell(i, rawdata, time1, time2, time3, time4, time5, threshold))
          assign(paste("B",i, sep=""), visioncounts_perwell(i+12, rawdata, time1, time2, time3, time4, time5, threshold))
          assign(paste("C",i, sep=""), visioncounts_perwell(i+24, rawdata, time1, time2, time3, time4, time5, threshold))
          assign(paste("D",i, sep=""), visioncounts_perwell(i+36, rawdata, time1, time2, time3, time4, time5, threshold))
          assign(paste("E",i, sep=""), visioncounts_perwell(i+48, rawdata, time1, time2, time3, time4, time5, threshold))
          assign(paste("F",i, sep=""), visioncounts_perwell(i+60, rawdata, time1, time2, time3, time4, time5, threshold))
          assign(paste("G",i, sep=""), visioncounts_perwell(i+72, rawdata, time1, time2, time3, time4, time5, threshold))
          assign(paste("H",i, sep=""), visioncounts_perwell(i+84, rawdata, time1, time2, time3, time4, time5, threshold))
          incProgress(amount = i/12)
        }
        
        #create vector array by row
        A <- c(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)
        B <- c(B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12)
        C <- c(C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12)
        D <- c(D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, D11, D12)
        E <- c(E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12)
        F <- c(F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12)
        G <- c(G1, G2, G3, G4, G5, G6, G7, G8, G9, G10, G11, G12)
        H <- c(H1, H2, H3, H4, H5, H6, H7, H8, H9, H10, H11, H12)
        
        
        # average resposnes from each Row of individual fish
        A.m <- mean(A)
        B.m <- mean(B)
        C.m <- mean(C)
        D.m <- mean(D)
        E.m <- mean(E)
        F.m <- mean(F)
        G.m <- mean(G)
        H.m <- mean(H)
        
        # standard error of each row          
        A.se <- StdErr(A, length(A))
        B.se <- StdErr(B, length(B))
        C.se <- StdErr(C, length(C))
        D.se <- StdErr(D, length(D))
        E.se <- StdErr(E, length(E))
        F.se <- StdErr(F, length(F))
        G.se <- StdErr(G, length(G))
        H.se <- StdErr(H, length(H))
      })    
      
      # plot every row as an individual bar if user hasnt grouped rows
      if (inGroup == FALSE)
      {
        
        Means <- c(A.m, B.m, C.m, D.m, E.m, F.m, G.m, H.m)
        stErrs <- c(A.se, B.se, C.se, D.se, E.se, F.se, G.se, H.se)
        
        # bar plot the averages from individual rows   
        mp <- barplot(c(A.m, B.m, C.m, D.m, E.m, F.m, G.m, H.m), axes=FALSE, axisnames=FALSE, ylim=c(0,5), main="Average Responses Per Row")
        axis(1, labels=c("A", "B","C","D", "E","F","G","H"), at = mp)
        axis(2, at=seq(0,5, by=1))
        box()
        Means <- c(A.m, B.m, C.m, D.m, E.m, F.m, G.m, H.m)
        stErrs <- matrix(stErrs <- c(A.se, B.se, C.se, D.se, E.se, F.se, G.se, H.se),8)
        segments(mp, Means - stErrs, mp, Means + stErrs, lwd=2)
        segments(mp - 0.1, Means - stErrs, mp + 0.1, Means - stErrs, lwd=2)
        segments(mp - 0.1, Means + stErrs, mp + 0.1, Means + stErrs, lwd=2)
        
      }
      
    }
    # calling functions for only using 48 well plate, repeat of above with less column and rows
    else if (inPlateNum == 48){
      withProgress(message = "Rendering... Please Wait", value = 0, {   
        
        # make a loop to cycle through all 96 wells and assign it a variable based on Row (example: well 1 = A1, well 96 = H12)       
        for (i in 1:8)
        {
          
          assign(paste("A",i, sep=""), visioncounts_perwell(i, rawdata, time1, time2, time3, time4, time5, threshold))
          assign(paste("B",i, sep=""), visioncounts_perwell(i+8, rawdata, time1, time2, time3, time4, time5, threshold))
          assign(paste("C",i, sep=""), visioncounts_perwell(i+16, rawdata, time1, time2, time3, time4, time5, threshold))
          assign(paste("D",i, sep=""), visioncounts_perwell(i+24, rawdata, time1, time2, time3, time4, time5, threshold))
          assign(paste("E",i, sep=""), visioncounts_perwell(i+32, rawdata, time1, time2, time3, time4, time5, threshold))
          assign(paste("F",i, sep=""), visioncounts_perwell(i+40, rawdata, time1, time2, time3, time4, time5, threshold))
          incProgress(amount = i/8)
        }
        
        #create vector array by row
        A <- c(A1, A2, A3, A4, A5, A6, A7, A8)
        B <- c(B1, B2, B3, B4, B5, B6, B7, B8)
        C <- c(C1, C2, C3, C4, C5, C6, C7, C8)
        D <- c(D1, D2, D3, D4, D5, D6, D7, D8)
        E <- c(E1, E2, E3, E4, E5, E6, E7, E8)
        F <- c(F1, F2, F3, F4, F5, F6, F7, F8)
        
        
        # average resposnes from each Row of individual fish
        A.m <- mean(A)
        B.m <- mean(B)
        C.m <- mean(C)
        D.m <- mean(D)
        E.m <- mean(E)
        F.m <- mean(F)
        
        # standard error of each row          
        A.se <- StdErr(A, length(A))
        B.se <- StdErr(B, length(B))
        C.se <- StdErr(C, length(C))
        D.se <- StdErr(D, length(D))
        E.se <- StdErr(E, length(E))
        F.se <- StdErr(F, length(F))
      })    
      
      # plot every row as an individual bar if user hasnt grouped rows
      if (inGroup == FALSE)
      {
        
        Means <- c(A.m, B.m, C.m, D.m, E.m, F.m)
        stErrs <- c(A.se, B.se, C.se, D.se, E.se, F.se)
        
        # bar plot the averages from individual rows   
        mp <- barplot(c(A.m, B.m, C.m, D.m, E.m, F.m), axes=FALSE, axisnames=FALSE, ylim=c(0,5), main="Average Responses Per Row")
        axis(1, labels=c("A", "B","C","D", "E","F"), at = mp)
        axis(2, at=seq(0,5, by=1))
        box()
        Means <- c(A.m, B.m, C.m, D.m, E.m, F.m)
        stErrs <- matrix(stErrs <- c(A.se, B.se, C.se, D.se, E.se, F.se),6)
        segments(mp, Means - stErrs, mp, Means + stErrs, lwd=2)
        segments(mp - 0.1, Means - stErrs, mp + 0.1, Means - stErrs, lwd=2)
        segments(mp - 0.1, Means + stErrs, mp + 0.1, Means + stErrs, lwd=2)
        
      }
      
    }
    
    # Combine bars and plot based on users input
    if (inGroup == TRUE)
    {
      withProgress(message = "Grouping... Please Wait", value = 0, {
        # extract groups from user inputs into something useable and
        # account for not all groups being used
        if(is.null(input$g1) == FALSE)
        {
          g1 <- mget(input$g1)
          g1 <- unlist(g1)
        }
        else
          g1 <- NULL
        
        if (is.null(input$g2) == FALSE)
        {
          g2 <- mget(input$g2)
          g2 <- unlist(g2)
        }
        else
          g2 <- NULL
        
        if (is.null(input$g3) == FALSE)
        {
          g3 <- mget(input$g3)
          g3 <- unlist(g3)
        }
        else
          g3 <- NULL                      
        
        # calculate combiend averages
        g1.m <- mean(g1)
        g2.m <- mean(g2)
        g3.m <- mean(g3)
        
        
        # calculate standard errors
        g1.se <- StdErr(g1, length(g1))
        g2.se <- StdErr(g2, length(g2))
        g3.se <- StdErr(g3, length(g3))
        
        message("Completed StdErrs")
        
        # barplot with standard error of the mean for combined groups
        mp <- barplot(c(g1.m,g2.m,g3.m), axes=FALSE, axisnames=FALSE, ylim=c(0,5), col=c("red","blue", "green"), main="Average Responses Per Group", ylab="Average # of Responses")       
        axis(1, labels=c("Group 1", "Group 2","Group 3"), at = mp)
        axis(2, at=seq(0,5, by=1))
        box()
        Means <- c(g1.m, g2.m, g3.m)
        stErrs <- matrix(c(g1.se, g2.se, g3.se),3)
        segments(mp, Means - stErrs, mp, Means + stErrs, lwd=2)
        segments(mp - 0.1, Means - stErrs, mp + 0.1, Means - stErrs, lwd=2)
        segments(mp - 0.1, Means + stErrs, mp + 0.1, Means + stErrs, lwd=2)
        
      })
      
      # save lengths of groups, needed for next data table
      g1.l <- length(g1)
      g2.l <- length(g2)
      g3.l <- length(g3)
      
      # make data summary table for display to user
      output$tableDataSummary <- renderDataTable({             
        Groups <- c("Group 1", "Group 2", "Group 3")
        Average <- round(Means, digits=2)
        StdError <- round(stErrs, digits=2)
        n <- c(g1.l, g2.l, g3.l)
        
        df <- data.frame(Groups, Average, StdError, n)
        return(df)
        
      }, options = list(dom = 't'))
      
      
      
      # make a p-value table comparing between groups for the user
      output$tablePvalue <- renderDataTable({
        
        # prevent errors coming from unsed groups
        p.g1g2 <- (t.test(g1,g2))[3]
        
        # round p values or truncate
        if (p.g1g2 < 0.0001)
          p.g1g2 <- '<0.0001'
        else
          p.g1g2 <- round(as.numeric(p.g1g2), digits=4)
        
        # if group 3 exists
        if (is.null(g3) == FALSE) 
        {
          p.g1g3 <- (t.test(g1,g3))[3]
          p.g2g3 <- (t.test(g2,g3))[3]
          
          # round p values or truncate
          if (p.g1g3 < 0.0001)
            p.g1g3 <- '<0.0001'
          else
            p.g1g3 <- round(as.numeric(p.g1g3), digits=4)
          
          # round p values or truncate
          if (p.g2g3 < 0.0001)
            p.g2g3 <- '<0.0001'
          else
            p.g2g3 <- round(as.numeric(p.g2g3), digits=4)
          
          # set group 3 flag on
          G3 <- TRUE
        }
        
        else #group 3 doesnt exist
        {
          p.g1g3 <- "NA"
          p.g2g3 <- "NA"
          
          # group 3 flag off
          G3 <- 0
        }
        
        
        # decide if p-values including group 3 should be displayed
        #if no group 3 is present, no need to complicate GUI
        if (G3 == TRUE) 
        {
          pval <- data.frame(p.g1g2, p.g1g3, p.g2g3) 
          colnames(pval) <- c("Group 1 and 2", "Group 1 and 3", "Group 2 and 3")
          pval
        }
        else if (G3 == FALSE)
        {
          pval <- data.frame(p.g1g2) 
          colnames(pval) <- c("Group 1 and 2")
          pval
        }
        
      }, options = list(dom = 't'))
      
      
      # Render a Title for the p-values table
      output$ptitle <- renderUI({ 
        h4("p Values", align="center")
      })
      
      # render a Title for the data table
      output$datasummary <- renderUI({ 
        h4("Data Summary", align="center")
      })
      
    }
  })
  
  
  # create table showing the values for each fish
  output$ind_table <- renderDataTable({  
    # assign variables from the UI inputs
    inFile <- input$file
    inRow <- input$row
    threshold <- input$threshold
    individual <- input$individual
    inSample <- input$sample
#    inPlateNum <- input$plateSelect
    
    # make sure uploaded file is not NULL; prevents errors, but only if Sample Data is not being used
    if (inSample == FALSE){
      if (is.null(inFile))
        return(NULL)
    }
    
    progVal <- 0
    
    withProgress(message = "Generating Table... Please Wait", value = 0, {
      
      #check if using Sample Data or User supplied data    
      if (inSample == FALSE){
        rawdata <- read.delim(inFile$datapath, sep="\t", header = TRUE)
      }
      if (inSample == TRUE){
        rawdata <- read.delim("www/sampleData.XLS", sep="\t", header = TRUE)
      }
      
      # determine if this rawdata is from a 96 well plate or a 48 well plate
      if (rawdata[96,1] == 'w096'){
        inPlateNum <- 96
      } else if (rawdata[96,1] == 'w048'){
        inPlateNum <- 48
      }
      
      
      # this line of code exists to fix a bug in the viewpoint software which causes randomy wells to be "measured" after
      # the recording stop time 
      rawdata <- subset(rawdata, rawdata$end <= 1950)
      
      rawdata$wellnum <- 1:96
      
      # needed variables for time, originally were input by user, but hard-coding these creates a cleaner UI
      time1 <- 1800
      time2 <- 1830
      time3 <- 1860
      time4 <- 1890
      time5 <- 1920
      
      # create variables for counts of every well
      # TODO: Create the loop repetition as a function where you just input the row letter
      
      if (inPlateNum == 96){
        for (i in 1:12)
        {
          resp <- visioncounts_perwell(i, rawdata, time1, time2, time3, time4, time5, threshold)
          if (is.null(resp)){
            resp <- "NA"
          }
          assign(paste("A",i, sep=""), resp)
        }
        rowA <- c(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12)
        incProgress(amount = 1/8)
        
        for (i in 1:12)
        {
          resp <- visioncounts_perwell(i+12, rawdata, time1, time2, time3, time4, time5, threshold)
          if (is.null(resp)){
            resp <- "NA"
          }
          assign(paste("B",i, sep=""), resp)
        }
        rowB <- c(B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12)
        incProgress(amount = 2/8)
        
        for (i in 1:12)
        {
          resp <- visioncounts_perwell(i+24, rawdata, time1, time2, time3, time4, time5, threshold)
          if (is.null(resp)){
            resp <- "NA"
          }
          assign(paste("C",i, sep=""), resp)
        }
        rowC <- c(C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12)
        incProgress(amount = 3/8)
        
        
        for (i in 1:12)
        {
          resp <- visioncounts_perwell(i+36, rawdata, time1, time2, time3, time4, time5, threshold)
          if (is.null(resp)){
            resp <- "NA"
          }
          assign(paste("D",i, sep=""), resp)
        }
        rowD <- c(D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,D11,D12)
        incProgress(amount = 4/8)
        
        
        for (i in 1:12)
        {
          resp <- visioncounts_perwell(i+48, rawdata, time1, time2, time3, time4, time5, threshold)
          if (is.null(resp)){
            resp <- "NA"
          }
          assign(paste("E",i, sep=""), resp)
        }
        rowE <- c(E1,E2,E3,E4,E5,E6,E7,E8,E9,E10,E11,E12)
        incProgress(amount = 5/8)
        
        
        for (i in 1:12)
        {
          resp <- visioncounts_perwell(i+60, rawdata, time1, time2, time3, time4, time5, threshold)
          if (is.null(resp)){
            resp <- "NA"
          }
          assign(paste("F",i, sep=""), resp)
        }
        rowF <- c(F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12)
        incProgress(amount = 6/8)
        
        for (i in 1:12)
        {
          resp <- visioncounts_perwell(i+72, rawdata, time1, time2, time3, time4, time5, threshold)
          if (is.null(resp)){
            resp <- "NA"
          }
          assign(paste("G",i, sep=""),resp)
        }
        rowG <- c(G1,G2,G3,G4,G5,G6,G7,G8,G9,G10,G11,G12)
        incProgress(amount = 7/8)
        
        
        for (i in 1:12)
        {
          resp <- visioncounts_perwell(i+84, rawdata, time1, time2, time3, time4, time5, threshold)
          if (is.null(resp)){
            resp <- "NA"
          }
          assign(paste("H",i, sep=""), resp)
        }
        rowH <- c(H1,H2,H3,H4,H5,H6,H7,H8,H9,H10,H11,H12)
        incProgress(amount = 1)
        
        
        wells <- c("Well 1", "Well 2", "Well 3", "Well 4", "Well 5", "Well 6", "Well 7", "Well 8", "Well 9", "Well 10", "Well 11", "Well 12")
        df <- data.frame(wells, rowA,rowB,rowC,rowD,rowE,rowF,rowG,rowH)
        
      } else if (inPlateNum == 48){
        for (i in 1:8)
        {
          resp <- visioncounts_perwell(i, rawdata, time1, time2, time3, time4, time5, threshold)
          if (is.null(resp)){
            resp <- "NA"
          }
          assign(paste("A",i, sep=""), resp)
        }
        rowA <- c(A1,A2,A3,A4,A5,A6,A7,A8)
        incProgress(amount = 1/6)
        
        for (i in 1:8)
        {
          resp <- visioncounts_perwell(i+8, rawdata, time1, time2, time3, time4, time5, threshold)
          if (is.null(resp)){
            resp <- "NA"
          }
          assign(paste("B",i, sep=""), resp)
        }
        rowB <- c(B1,B2,B3,B4,B5,B6,B7,B8)
        incProgress(amount = 2/6)
        
        for (i in 1:8)
        {
          resp <- visioncounts_perwell(i+16, rawdata, time1, time2, time3, time4, time5, threshold)
          if (is.null(resp)){
            resp <- "NA"
          }
          assign(paste("C",i, sep=""), resp)
        }
        rowC <- c(C1,C2,C3,C4,C5,C6,C7,C8)
        incProgress(amount = 3/6)
        
        
        for (i in 1:8)
        {
          resp <- visioncounts_perwell(i+24, rawdata, time1, time2, time3, time4, time5, threshold)
          if (is.null(resp)){
            resp <- "NA"
          }
          assign(paste("D",i, sep=""), resp)
        }
        rowD <- c(D1,D2,D3,D4,D5,D6,D7,D8)
        incProgress(amount = 4/6)
        
        
        for (i in 1:8)
        {
          resp <- visioncounts_perwell(i+32, rawdata, time1, time2, time3, time4, time5, threshold)
          if (is.null(resp)){
            resp <- "NA"
          }
          assign(paste("E",i, sep=""), resp)
        }
        rowE <- c(E1,E2,E3,E4,E5,E6,E7,E8)
        incProgress(amount = 5/6)
        
        
        for (i in 1:8)
        {
          resp <- visioncounts_perwell(i+40, rawdata, time1, time2, time3, time4, time5, threshold)
          if (is.null(resp)){
            resp <- "NA"
          }
          assign(paste("F",i, sep=""), resp)
        }
        rowF <- c(F1,F2,F3,F4,F5,F6,F7,F8)
        incProgress(amount = 1)
        
        
        
        wells <- c("Well 1", "Well 2", "Well 3", "Well 4", "Well 5", "Well 6", "Well 7", "Well 8")
        df <- data.frame(wells, rowA,rowB,rowC,rowD,rowE,rowF)
        
        
        
      }
    })
    
    ## Allow downloading of CSV file of df table, this must be within the scope of the generated df
    output$downloadData <- downloadHandler(
      filename = paste(Sys.time(),'csv', sep = "."),
      content = function(file) {
        write.table(x = df, file, sep = ",", row.names = FALSE)
      }
    )
    
    ## display the df on the page, needed after download handler
    return (df)
    
  })
  
  
  # renders the changelog HTML file as a reactive variable to be displayed in the changelog tab in the UI
  # allows the changelog to be kept in a file outside of the R scripts
  output$changelog <- renderUI({
    HTML(readLines("www/changelog.html", warn = FALSE))
    
  })
  
  # render the intstructions tab from an html file
  #allows the intro to be kept in a file outside of the R scripts
  output$intro <- renderUI({
    HTML(readLines("www/intro.html", warn = FALSE))
  })
  
  output$beginAndEnd <- renderDataTable({
    inFile <- input$file
    inSample <- input$sample
    
    if (inSample == FALSE){
      if(is.null(inFile)){
        return (NULL)
      } 
      rawdata <- read.delim(inFile$datapath, sep="\t", header = TRUE)
    }
    else if (inSample == TRUE){
      rawdata <- read.delim("www/sampleData.XLS", sep="\t", header = TRUE)
    }
    timeList <- as.vector(rawdata$sttime)
    remove(rawdata)
    
    startAndStopTimes <- experimentTimes(timeList)
    labels <- c("Experiment Start Time", "Experiment Stop Time")
    
    df <- cbind(labels,startAndStopTimes)
    colnames(df, do.NULL = FALSE)
    colnames(df) <- c("","")
    df
    
  }, options = list(dom = 't'))
  
  
  
  
  
  
  
  
})





