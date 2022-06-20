library(tictoc)
library(Bolstad2)

getSamplesInSaccade <- function(index) {
  # Return a filtered dataframe containing only the samples whose timestamp 
  # lies within the start and end times of the saccade specified by index
  
  ssacc = gaze$saccades$sttime[index]
  esacc = gaze$saccades$entime[index]
  
  samples <- gaze$samples
  
  samples[samples$time <= esacc & samples$time >= ssacc,]
  # samples %>% filter(time <= esacc & time >= ssacc)
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
  
  amplitude <- sqrt(x_dist*x_dist + y_dist*y_dist)
}

getAmplitude2 <- function(samples, base_sample) {
  all_gazeX1 <- samples$gxR
  all_gazeY1 <- samples$gyR
  all_resX1 <- samples$rx
  all_resY1 <- samples$ry
  
  all_gazeX2 <- base_sample$gxR
  all_gazeY2 <- base_sample$gyR
  all_resX2 <- base_sample$rx
  all_resY2 <- base_sample$ry
  
  all_x_dist <- (all_gazeX1 - all_gazeX2) / mean(all_resX1, all_resX2)
  all_y_dist <- (all_gazeY1 - all_gazeY2) / mean(all_resY1, all_resY2)
  
  all_amplitudes <- sqrt(all_x_dist*all_x_dist + all_y_dist*all_y_dist)
}

getAngle <- function(saccadeSlope,sampleSlope){
  # Return angle between two lines based on their slopes
  atan(abs((sampleSlope-saccadeSlope)/(1+saccadeSlope*sampleSlope)))*180/pi
}

getAngle2 <- function(slopes, base_slope){
  # Return angle between two lines based on their slopes
  atan(abs((slopes-base_slope)/(1+base_slope*slopes)))*180/pi
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

getSlope2 <- function(samples, base_sample){
  # Compute the slope of a line between to points
  all_gX1 <- samples$gxR
  all_gY1 <- samples$gyR
  gX2 <- base_sample$gxR
  gY2 <- base_sample$gyR
  all_diffx <- gX2-all_gX1
  all_diffy <- gY2-all_gY1
  all_diffx/all_diffy
}

getOrthogonalDistance <- function(firstSample,lastSample, currentSample){
  # Compute the orthogonal distance between a sample and the straight line between first and last samples
  x1 <-firstSample$gxR
  y1 <-firstSample$gyR
  x2 <-lastSample$gxR
  y2 <-lastSample$gyR
  x0 <-currentSample$gxR
  y0 <-currentSample$gyR
  abs((x2-x1)*(y1-y0)-(x1-x0)*(y2-y1))/sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1))
}

getOrthogonalDistance2 <- function(firstSample,lastSample, samples){
  # Compute the orthogonal distance between a sample and the straight line between first and last samples
  x1 <-firstSample$gxR
  y1 <-firstSample$gyR
  x2 <-lastSample$gxR
  y2 <-lastSample$gyR
  all_x0 <-samples$gxR
  all_y0 <-samples$gyR
  abs((x2-x1)*(y1-all_y0)-(x1-all_x0)*(y2-y1))/sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1))
}