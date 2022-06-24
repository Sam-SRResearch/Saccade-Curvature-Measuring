library(tictoc)
library(Bolstad2)

getAmplitude <- function(samples, base_sample, eye_val) {
  gx_val <- eye_val['gx']
  gy_val <- eye_val['gy']
  
  all_gazeX1 <- samples[[gx_val]]
  all_gazeY1 <- samples[[gy_val]]
  all_resX1 <- samples$rx
  all_resY1 <- samples$ry
  
  all_gazeX2 <- base_sample[[gx_val]]
  all_gazeY2 <- base_sample[[gy_val]]
  all_resX2 <- base_sample$rx
  all_resY2 <- base_sample$ry
  
  all_x_dist <- (all_gazeX1 - all_gazeX2) / mean(all_resX1, all_resX2)
  all_y_dist <- (all_gazeY1 - all_gazeY2) / mean(all_resY1, all_resY2)
  
  all_amplitudes <- sqrt(all_x_dist*all_x_dist + all_y_dist*all_y_dist)
}

getAngle <- function(slopes, base_slope){
  # Return angle between two lines based on their slopes
  atan(abs((slopes-base_slope)/(1+base_slope*slopes)))*180/pi
}

getSlope <- function(samples, base_sample, eye_val){
  # Compute the slope of a line between to points
  gx_val <- eye_val['gx']
  gy_val <- eye_val['gy']
  
  all_gX1 <- samples[[gx_val]]
  all_gY1 <- samples[[gy_val]]
  gX2 <- base_sample[[gx_val]]
  gY2 <- base_sample[[gy_val]]
  all_diffx <- gX2-all_gX1
  all_diffy <- gY2-all_gY1
  all_diffx/all_diffy
}

getOrthogonalDistance <- function(firstSample,lastSample, samples, eye_val){
  # Compute the orthogonal distance between a sample and the straight line between first and last samples
  gx_val <- eye_val['gx']
  gy_val <- eye_val['gy']
  
  x1 <-firstSample[[gx_val]]
  y1 <-firstSample[[gy_val]]
  x2 <-lastSample[[gx_val]]
  y2 <-lastSample[[gy_val]]
  all_x0 <-samples[[gx_val]]
  all_y0 <-samples[[gy_val]]
  abs((x2-x1)*(y1-all_y0)-(x1-all_x0)*(y2-y1))/sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1))
}

getMainSaccAngle <- function(firstSample, lastSample, eye_val){
  # Compute the angle of the main saccade trajectory (straight line between first and last sample)
  # compared to horizontal,using Data Viewer convention of 0-180 and 0- -180
  gx_val <- eye_val['gx']
  gy_val <- eye_val['gy']
  
  xdiff <- last_sample[[gx_val]] - first_sample[[gx_val]]
  ydiff <- -(last_sample[[gy_val]] - first_sample[[gy_val]]) #Invert because  top left pixel coords
  saccAngle <- atan(ydiff/xdiff)*180/pi
  if(xdiff<0 && ydiff >0){
    saccAngle <- saccAngle +180
  }
  if(xdiff<0 && ydiff <0){
    saccAngle <- saccAngle -180
  }
 saccAngle 
}

checkAmp <- function(samples, min_amp, eye_val){
  first_sample <- samples[1] # get first sample of saccade
  last_sample <- tail(samples, 1) # get last sample of saccade
  
  amp <- getAmplitude(first_sample, last_sample, eye_val)
  
  amp >= min_amp
}