library(Bolstad2)
library(dplyr)

source("./helpers.R")

measureAll <- function(samples, eye_val) {
  # apply all measures
  first_sample <- samples[1] # get first sample of saccade
  last_sample <- tail(samples, 1) # get last sample of saccade
  
  odists <- getOrthogonalDistance(first_sample, last_sample, samples, eye_val)
  num_rows <- nrow(samples)
  
  
  first_slope <- getSlope(last_sample, first_sample, eye_val) # get base slope
  famplitudes <- getAmplitude(samples, first_sample, eye_val)
  lamplitudes <- getAmplitude(samples, last_sample, eye_val)
  
  samples <- samples[famplitudes >= 0.5 & lamplitudes >= 0.5] # filter out samples with amplitudes >= 0.5
  slopes <- getSlope(samples, first_sample, eye_val)
  angles <- getAngle(slopes, first_slope)
  
  c('median_angles'=median(angles[!is.na(angles)]), 
    'mean_odists'=mean(odists, na.rm=TRUE),
    'max_odists'=max(odists),
    'area'=sintegral(1:num_rows, odists, num_rows)$int)
}

measureAngle <- function(samples, eye_val) {
  # return median of the angles between line between first and current sample
  # and line between first and last sample
  
  first_sample <- samples[1] # get first sample of saccade
  last_sample <- tail(samples, 1) # get last sample of saccade
  
  first_slope <- getSlope(last_sample, first_sample, eye_val) # get base slope

  famplitudes <- getAmplitude(samples, first_sample, eye_val)
  lamplitudes <- getAmplitude(samples, last_sample, eye_val)
  
  samples <- samples[famplitudes >= 0.5 & lamplitudes >= 0.5] # filter out samples with amplitudes >= 0.5
  
  slopes <- getSlope(samples, first_sample, eye_val)
  angles <- getAngle(slopes, first_slope)
  
  c('median_angles'=median(angles[!is.na(angles)])) # get median (filtering out NA's first)
}

measureOrtho <- function(samples, eye_val) {
  # return a vector with the mean and max of the orthogonal distances between 
  # each sample and the line between first and last sample
  first_sample <- samples[1] # store the starting sample
  last_sample <- tail(samples, 1) # store the last sample
  
  odists <- getOrthogonalDistance(first_sample, last_sample, samples, eye_val)

  c('mean_odists'=mean(odists, na.rm=TRUE),'max_odists'=max(odists)) # get mean and max orthogonal distance in pixels: TO DO: turn to degs of VA
}

measureArea <- function(samples, eye_val) {
  # return area under the curve of the saccade
  first_sample <- samples[1] # store the starting sample
  last_sample <- tail(samples ,1) # store the last sample
  
  odists <- getOrthogonalDistance(first_sample, last_sample, samples, eye_val)
  
  num_rows <- nrow(samples)
  
  c('area'=sintegral(1:num_rows, odists, num_rows)$int)
}
