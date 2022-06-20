library(eyelinkReader)
library(data.table)

source("./measures.R")

gaze <- read_edf('data/14.edf', import_recordings=FALSE, 
                 import_saccades = TRUE, import_blinks=FALSE, 
                 import_fixations=FALSE, import_variables=FALSE, 
                 sample_attributes = c('time', 'gx', 'gy', 'rx', 'ry'))

applyToAll <- function(metric=1) {
  measures <- list('measureAll', 'measureAngle', 'measureOrtho', 'measureArea')
  
  num_row <- nrow(gaze$saccades)
  vec <<- rep(NA, num_row)
  
  message("Calculating curvature metrics... (this may take a minute)")
  
  for (row_index in 1:num_row) {
    sample_df <<- getSamplesInSaccade(row_index)

    if (!anyNA(sample_df$gxR)) {
      vec[row_index] <- do.call(measures[[metric]], list())
    }
  }
  vec
}

applyToAll2 <- function(metric=0) {
  measures <- list('measureAll', 'measureAngle2', 'measureOrtho2', 'measureArea')

  all_samples <<- setDT(gaze$samples)
  all_saccades <<- setDT(gaze$saccades)
  
  saccade_count <- nrow(all_saccades)
  
  return_table <- all_saccades %>% select(trial, sttime, entime, duration)
  
  if (metric == 0) {
    
    print('measureAll not implemented')
    
  } else if (metric == 1) {
    
    median_angle <- rep(NA, nrow(all_saccades))
    
    for (saccade_index in 1:nrow(all_saccades)) {   # loop through all saccades
      saccade <- all_saccades[saccade_index] # get current saccade
      saccade_samples <- all_samples[time >= saccade$sttime & time <=saccade$entime] # collect all samples in saccade
      
      if (!anyNA(saccade_samples$gxR)) {
        median_angles[saccade_index] <- measureAngle2(saccade_samples)
      }
    }
    return_table %>% cbind(median_angles)
    
  } else if (metric == 2) {
    
    mean_odist <- rep(NA, nrow(all_saccades))
    max_odist <- rep(NA, nrow(all_saccades))
    
    for (saccade_index in 1:nrow(all_saccades)) {   # loop through all saccades
      saccade <- all_saccades[saccade_index] # get current saccade
      saccade_samples <- all_samples[time >= saccade$sttime & time <=saccade$entime] # collect all samples in saccade
      
      if (!anyNA(saccade_samples$gxR)) {
        results <- measureOrtho2(saccade_samples)
        # print(results)
        mean_odists[saccade_index] <- results[1]
        max_odists[saccade_index] <- results[2]
      }
    }
    return_table %>% cbind(mean_odists, max_odists)
    
  } else if (metric == 3) {
    
    area <- rep(NA, nrow(all_saccades))
    
    for (saccade_index in 1:nrow(all_saccades)) {   # loop through all saccades
      saccade <- all_saccades[saccade_index] # get current saccade
      saccade_samples <- all_samples[time >= saccade$sttime & time <=saccade$entime] # collect all samples in saccade
      
      if (!anyNA(saccade_samples$gxR)) {
        results <- measureArea2(saccade_samples)
        area[saccade_index] <- results
      }
    }
    return_table %>% cbind(area)
    
  }
}

# Test Loop 
# Fix for jim.edf There seems to be some bug in the EDF reader - all the saccades are in trial 1
# num_row <-  nrow(gaze$saccades[gaze$saccades$trial==1,]) 
num_row <- nrow(gaze$saccades)
resultsTable <-  matrix(NA, nrow=num_row, ncol=5) # Prepare table
for (row_index in 1:num_row) {
  sample_df <<- getSamplesInSaccade(row_index)
  ampcheck <- getAmplitude(sample_df[1,],tail(sample_df,1))
  if (sum(is.na(sample_df$gxR)) == 0 & ampcheck >= 2) {
    resultsTable[row_index,] <- measureAll()
  }
}

# Attempt radial plot
library('ggplot2')
breaks = c(0,15,45,75,105,135,165,195,225,255,285,315,345,360) # Make breaks including half-sized first / last breaks
fortyfive = c(0,45,90,135,180,225,270,315)                     # For major gridlines
ninety = c(0,90,180,270)                                       # For red lines

# Code to classify each saccade by angle bin
df <- as.data.frame(resultsTable)
df$angles <- as.numeric(df$V4+360)%%360 # Make +/- 180 ((data into 0-360 deg data
df$anglebin <- as.numeric(cut(df$angles, breaks=breaks, right=TRUE))
colnames(df) <- c("MedSlope","MeanOrth","MaxOrth","trial","Angle","Angle360","AngleBin")
df$AngleBin[df$AngleBin==13] <- 1 #Merge the first / last half sized segments
binmeans <-aggregate(df$MedSlope, list(df$AngleBin), mean, na.rm=TRUE) #change MedSlope for MeanOrth etc
binmeans <- binmeans[,2]

plotdf <- data.frame(x = seq(0,359,30),y = binmeans)                        # Combine into data frame
plot<- ggplot(plotdf, aes(x,y)) +
  geom_col(colour = "black",fill = "light blue", alpha = 0.5) +  # make plot
  scale_x_continuous(breaks = fortyfive,limits = c(-15, 345)) +  # shift limits by 15 degrees
  geom_vline(xintercept = ninety, colour = "red") +              # draw red cross hairs
  coord_polar(start = -105/360*2*pi, direction = -1)           # turn cols to polar and offset by 105 degrees (converted to radians) so that 0 is East
plot


# Code to plot Numbmer of saccades as function of angles
angle_count <- as.data.frame(table(df$AngleBin))                          #count freq of saccades at different angles
                                                 
plotdf <- data.frame(x = seq(0,359,30),y = angle_count$Freq)                        # Combine into data frame
plot<- ggplot(plotdf, aes(x,y)) +
  geom_col(colour = "black",fill = "light blue", alpha = 0.5) +  # make plot
  scale_x_continuous(breaks = fortyfive,limits = c(-15, 345)) +  # shift limits by 15 degrees
  geom_vline(xintercept = ninety, colour = "red") +              # draw red cross hairs
  coord_polar(start = -105/360*2*pi, direction = -1)           # turn cols to polar and offset by 105 degrees (converted to radians) so that 0 is East
plot
