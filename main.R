#=====================================================================
# Saccade Curvature Measurement Script
# (R-language Version 4.2.1)
# Authors: Jeffery Zhan, Sam Hutton (SR-Research Ltd.)
# Date: July 15, 2022
#
# The following scripts takes in EyeLink data as .edf files, 
# and returns a table, each row representing a saccade, with the following columns: 
# trial number, start time, end time, duration, eye, amplitude,
# and depending on input parameters, median angle, mean orthogonal distance and
# max orthogonal distance, total area under the curve, or all 3.
#=====================================================================

#-------------------------------------------------------
# 1. Initialization Block -
# Clear environment / load in library and function files
#-------------------------------------------------------
library(eyelinkReader)
library(data.table)

rm(list = ls())

source("./measures.R")

#-------------------------------------------------------
# 2. Function Block -
# The main function that will be used
#-------------------------------------------------------

applyToAll <- function(metric=0, min_amp=2) {
  # Return a datatable, each row representing a saccade, with the following columns:
  # trial number, start time, end time, duration, eye, amplitude - 
  # And, depending on input parameters, median angle, mean orthogonal distance 
  # and max orthogonal distance, total area under the curve, or all three.
  #
  # This function takes two optional arguments: metric, and min_amp.
  # 
  # metric changes which metrics should be calculated and included in the final table.
  # By default, the value of metric is 0.
  # 0 = all metrics
  # 1 = median angle
  # 2 = mean and max orthogonal distance
  # 3 = total area under the curve
  # 
  # min_amp changes the threshold that saccade amplitudes should be greater or equal to
  # in order to be calculated. Any saccades with amplitudes lower than this value will have
  # NA metrics in the returned table.
  # By default, the value of min_amp is 2.
  
  gaze <- read_edf(file.choose(), import_recordings=FALSE, 
                   import_saccades = TRUE, import_blinks=FALSE, 
                   import_fixations=FALSE, import_variables=FALSE, 
                   sample_attributes = c('time', 'gx', 'gy', 'rx', 'ry'))
  
  all_samples <- setDT(gaze$samples)
  all_saccades <- setDT(gaze$saccades)
  # convert from dataframe to datatable mainly for speed and convenience
  
  saccade_count <- nrow(all_saccades)
  
  return_table <- all_saccades %>% select(trial, sttime, entime, duration, eye)

  median_angle <- rep(NA, nrow(all_saccades))
  mean_odist <- rep(NA, nrow(all_saccades))
  max_odist <- rep(NA, nrow(all_saccades))
  area <- rep(NA, nrow(all_saccades))
  amplitude <- rep(NA, nrow(all_saccades))
  
  for (saccade_index in 1:nrow(all_saccades)) {   # loop through all saccades
    saccade <- all_saccades[saccade_index] # get current saccade
    saccade_samples <- all_samples[time >= saccade$sttime & time <=saccade$entime] # collect all samples in saccade
    
    eye_val <- if (saccade$eye == 'LEFT') {   # for working with data from both eyes
      c(gx='gxL', gy='gyL')
    } else {
      c(gx='gxR', gy='gyR')
    }

    amp <- checkAmp(saccade_samples, eye_val)
    
    if ((amp >= min_amp) & 
        !anyNA(saccade_samples[[eye_val['gx']]]) & 
        !anyNA(saccade_samples[[eye_val['gy']]])) {
      
      amplitude[saccade_index] <- amp
      
      if (metric == 0) {
        results <- measureAll(saccade_samples, eye_val)
        median_angle[saccade_index] <- results['median_angles']
        mean_odist[saccade_index] <- results['mean_odists']
        max_odist[saccade_index] <- results['max_odists']
        area[saccade_index] <- results['area']

      } else if (metric == 1) {
        results <- measureAngle(saccade_samples, eye_val)
        median_angle[saccade_index] <- results['median_angles']
        
      } else if (metric == 2) {
        results <- measureOrtho(saccade_samples, eye_val)
        mean_odist[saccade_index] <- results['mean_odists']
        max_odist[saccade_index] <- results['max_odists']
        
      } else if (metric == 3) {
        results <- measureArea(saccade_samples, eye_val)
        area[saccade_index] <- results['area']
        
      }
    }
  }
  return_table %>% cbind(amplitude, median_angle, mean_odist, max_odist, area)
}

# 
# # Attempt radial plot
# library('ggplot2')
# breaks = c(0,15,45,75,105,135,165,195,225,255,285,315,345,360) # Make breaks including half-sized first / last breaks
# fortyfive = c(0,45,90,135,180,225,270,315)                     # For major gridlines
# ninety = c(0,90,180,270)                                       # For red lines
# 
# # Code to classify each saccade by angle bin
# df <- as.data.frame(resultsTable)
# df$angles <- as.numeric(df$V4+360)%%360 # Make +/- 180 ((data into 0-360 deg data
# df$anglebin <- as.numeric(cut(df$angles, breaks=breaks, right=TRUE))
# colnames(df) <- c("MedSlope","MeanOrth","MaxOrth","trial","Angle","Angle360","AngleBin")
# df$AngleBin[df$AngleBin==13] <- 1 #Merge the first / last half sized segments
# binmeans <-aggregate(df$MedSlope, list(df$AngleBin), mean, na.rm=TRUE) #change MedSlope for MeanOrth etc
# binmeans <- binmeans[,2]
# 
# plotdf <- data.frame(x = seq(0,359,30),y = binmeans)                        # Combine into data frame
# plot<- ggplot(plotdf, aes(x,y)) +
#   geom_col(colour = "black",fill = "light blue", alpha = 0.5) +  # make plot
#   scale_x_continuous(breaks = fortyfive,limits = c(-15, 345)) +  # shift limits by 15 degrees
#   geom_vline(xintercept = ninety, colour = "red") +              # draw red cross hairs
#   coord_polar(start = -105/360*2*pi, direction = -1)           # turn cols to polar and offset by 105 degrees (converted to radians) so that 0 is East
# plot
# 
# 
# # Code to plot Numbmer of saccades as function of angles
# angle_count <- as.data.frame(table(df$AngleBin))                          #count freq of saccades at different angles
#                                                  
# plotdf <- data.frame(x = seq(0,359,30),y = angle_count$Freq)                        # Combine into data frame
# plot<- ggplot(plotdf, aes(x,y)) +
#   geom_col(colour = "black",fill = "light blue", alpha = 0.5) +  # make plot
#   scale_x_continuous(breaks = fortyfive,limits = c(-15, 345)) +  # shift limits by 15 degrees
#   geom_vline(xintercept = ninety, colour = "red") +              # draw red cross hairs
#   coord_polar(start = -105/360*2*pi, direction = -1)           # turn cols to polar and offset by 105 degrees (converted to radians) so that 0 is East
# plot
