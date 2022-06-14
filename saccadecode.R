library(eyelinkReader)
library(dplyr)
library(tictoc)
gaze <- read_edf('data/jim.edf',import_samples=TRUE)

doSomethingWithSample <- function(index) {
  # do something with sample
  sample_df <- getSamplesInSaccade(index)
}

applyToAll <- function(measureName) {
  num_row <- nrow(gaze$saccades)
  
  vec <- rep(NA, num_row)
  
  if (measureName == 'angle') {
    for (row_index in 1:num_row) {
      vec[row_index] <- measureSaccAngles(row_index)
    }
    return(vec)
  } else if (measureName == 'area') {
    print('area')
  }
}

measureSaccAngles <- function(index) {
  # measure angles between line between first and current sample
  # and line between first and last sample
  sample_df <<- getSamplesInSaccade(index)
  if (sum(is.na(sample_df$gxR)) > 0) {
    print(paste("Index", index, ": Blink detected, ignoring..."), sep=" ")
  } else {
    print(paste("Index", index, ": Calculating..."), sep=" ")
    first_sample <- sample_df[1,] # store the starting sample
    last_sample <- tail(sample_df,1) # store the last sample
    last_sample_amp <- getAmplitude(last_sample, first_sample)
    
    sacc_slope <- getSlope(first_sample,last_sample)
    
    num_rows <- nrow(sample_df)
    
    angles <- rep(NA, num_rows) # initialize vector to fill in with angle values
    
    for (row_index in 1:num_rows) {
      sample <- sample_df[row_index, ] # get sample for current index
      amplitude <- getAmplitude(sample, first_sample)
      
      if (amplitude >= 0.5 || last_sample_amp - amplitude >= 0.5) {
        slope <- getSlope(first_sample,sample)
        angle <- getAngle(sacc_slope, slope)
        
        angles[row_index] <- angle
      }
    }
    median(angles[!is.na(angles)]) # get median (filtering out NA's first)
  }
}

getSamplesInSaccade <- function(index) {
  # Return a filtered dataframe containing only the samples whose timestamp 
  # lies within the start and end times of the saccade specified by index
  
  ssacc = gaze$saccades$sttime[index]
  esacc = gaze$saccades$entime[index]
  
  gaze$samples[gaze$samples$time <= esacc & gaze$samples$time >= ssacc,]
}

getAmplitude <- function(sample1, sample2) {
  # Return the amplitude, in degrees of visual angle, between sample1 and sample2
  gazeX1 <- sample1$gxR
  gazeY1 <- sample1$gyR
  resX1 <- sample1$rx
  resY1 <- sample1$ry
  
  gazeX2 <- sample2$gxR
  gazeY2 <- sample2$gyR
  resX2 <- sample2$rx
  resY2 <- sample2$ry
  
  x_dist <- (gazeX1 - gazeX2) / mean(resX1, resX2)
  y_dist <- (gazeY1 - gazeY2) / mean(resY1, resY2)
  
  amplitude <- sqrt(x_dist^2 + y_dist^2)
}

getAngle <- function(saccadeSlope,sampleSlope){
  # Return angle between two lines based on their slopes
  atan(abs((sampleSlope-saccadeSlope)/(1+saccadeSlope*sampleSlope)))*180/pi
}

getSlope <- function(firstSample,lastSample){
  # Compute the slope of a line between to points
  gX1 <- firstSample$gxR
  gY1 <- firstSample$gyR
  gX2 <- lastSample$gxR
  gY2 <- lastSample$gyR
  diffx <- gX2-gX1
  diffy <- gY2-gY1
  diffx/diffy
}
