library(eyelinkReader)
library(data.table)

source("./measures.R")

gaze <- read_edf('data/jim.edf', import_recordings=FALSE, 
                 import_saccades = TRUE, import_blinks=FALSE, 
                 import_fixations=FALSE, import_variables=FALSE, 
                 sample_attributes = c('time', 'gx', 'gy', 'rx', 'ry'))

applyToAll <- function(metric=1) {
  measures <- list('measureAll', 'measureAngle', 'measureOrtho', 'measureArea')
  
  num_row <- nrow(gaze$saccades)
  vec <- rep(NA, num_row)
  
  message("Loading... (this may take a minute)")
  
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
