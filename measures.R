library(Bolstad2)
library(dplyr)

source("./helpers.R")

measureAll <- function() {
  # apply all measures

  first_sample <<- sample_df[1,] # store the starting sample
  last_sample <<- tail(sample_df,1) # store the last sample
  last_sample_amp <- getAmplitude(last_sample, first_sample)
  
  #TODO: if last_sample_amp < 2.0 degs then skip all the below
  
  sacc_slope <- getSlope(first_sample,last_sample)
  mainSaccAngle <- getMainSaccAngle(first_sample,last_sample) # why does this only work if first/last_sample is <<-?
  
  num_rows <- nrow(sample_df)
  
  angles <- rep(NA, num_rows) # initialize vector to fill in with angle values
  odists <- rep(NA, num_rows) # initialize vector to fill in with odist values

  for (row_index in 1:num_rows) {
    
    sample <- sample_df[row_index, ] # get sample for current index
    famplitude <- getAmplitude(sample, first_sample)
    lamplitude <- getAmplitude(sample, last_sample)
    odist <- getOrthogonalDistance(first_sample,last_sample,sample)
    odists[row_index] <- odist
    if (famplitude >= 0.5 & lamplitude >= 0.5) {
      slope <- getSlope(first_sample,sample)
      angle <- getAngle(sacc_slope, slope)
      
      angles[row_index] <- angle
    }
  }
  measures <- c(median(angles[!is.na(angles)]), # get median angle (filtering out NA's first)
    mean(odists[!is.na(odists)]),max(odists),mainSaccAngle, sample_df$trial[1]) # get mean and max orthogonal distance in pixels: TO DO: turn to degs of VA
  return(measures)
}

measureAngle <- function(index) {
  # return median of the angles between line between first and current sample
  # and line between first and last sample

  first_sample <- sample_df[1,] # store the starting sample
  last_sample <- tail(sample_df,1) # store the last sample
  last_sample_amp <- getAmplitude(last_sample, first_sample)
  
  sacc_slope <- getSlope(first_sample, last_sample)
  
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

measureOrtho <- function(index) {
  # return a vector with the mean and max of the orthogonal distances between 
  # each sample and the line between first and last sample
  sample_df <<- getSamplesInSaccade(index)
  
  if (sum(is.na(sample_df$gxR)) > 0) {
    print(paste("Index", index, ": Blink detected, ignoring..."), sep=" ")
  } else {
    print(paste("Index", index, ": Calculating..."), sep=" ")
    first_sample <<- sample_df[1,] # store the starting sample
    last_sample <<- tail(sample_df,1) # store the last sample

    num_rows <- nrow(sample_df)
    
    odists <- rep(NA, num_rows) # initialize vector to fill in with odist values
    
    for (row_index in 1:num_rows) {
      sample <- sample_df[row_index, ] # get sample for current index

      odist <- getOrthogonalDistance(first_sample,last_sample,sample)
      odists[row_index] <- odist
    }
    c('mean'=mean(odists[!is.na(odists)]),'max'=max(odists)) # get mean and max orthogonal distance in pixels: TO DO: turn to degs of VA
  }
}

measureArea <- function(index) {
  # return area under the curve of the saccade
  sample_df <<- getSamplesInSaccade(index)
  
  if (sum(is.na(sample_df$gxR)) > 0) {
    print(paste("Index", index, ": Blink detected, ignoring..."), sep=" ")
  } else {
    print(paste("Index", index, ": Calculating..."), sep=" ")
    first_sample <- sample_df[1,] # store the starting sample
    last_sample <- tail(sample_df,1) # store the last sample
    
    num_rows <- nrow(sample_df)
    
    odists <<- rep(NA, num_rows)
    
    for (row_index in 1:num_rows) {
      sample <- sample_df[row_index, ] # get sample for current index
      
      odist <- getOrthogonalDistance(first_sample,last_sample,sample)
      odists[row_index] <<- odist
    }
    
    sintegral(1:num_rows, odists, num_rows)
  }
}
