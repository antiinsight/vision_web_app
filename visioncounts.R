visioncounts_perwell <- function(a, rawdata, t1, t2, t3,t4, t5, threshold)
{
  
  ## a represents a well number on the plate from 1 to 96
  x <- a
  
  ## pull out all x animals
  animal <- subset(rawdata, wellnum == x)

  animal_dark <- subset(animal, start==t1 | start== t2 | start==t3 | start==t4 | start==t5, select=c("actinteg"))
  actinteg <- animal_dark[,1]
  
  ## calculate normalization based on overall activity levels
  Activity_AVG <- aggregate(animal$actinteg ~ animal$start, FUN=mean)
  #ignores the first 10 seconds in normalization
  Activity_AVG <- subset(Activity_AVG, Activity_AVG$`animal$start` > 10)
  
  timepoints <- Activity_AVG[,2]
  norm_avg <- mean(timepoints)
  
  ## return 0 if well is empty, this prevents errors later in this function
  if (norm_avg == 0)
  {
    return(NULL)
  }
  
    ## normalize actinteg
    actinteg_norm <- actinteg/norm_avg

    ## count responses
    counter <- 0
    
    for (i in 1:5)
    {
      if(actinteg_norm[i] >= threshold)
      {
        counter <- counter + 1
      }
    }
    
    return(counter)  

}

StdErr <- function(x, n)
{
  
  SD <- sd(x)
  SR <- sqrt(n)
  SE <- SD/SR
  
  return(SE)
}

experimentTimes <- function(input){
  startTime <- input[1]
  endTime <- input[length(input)]
  
  result <- c(startTime, endTime)
  
  return(result)
  
}


