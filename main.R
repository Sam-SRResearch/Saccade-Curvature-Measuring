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
  vec <<- rep(NA, num_row)
  
  message("Calculating curvature metrics... (this may take a minute)")
  
  for (row_index in 1:num_row) {
    sample_df <<- getSamplesInSaccade(row_index)
    if (sum(is.na(sample_df$gxR)) == 0) {
      vec[row_index,] <- do.call(measures[[metric]], list())
    }
  }
  vec
}

# Test Loop
num_row <- 20
resultsTable <-  matrix(NA, nrow=num_row, ncol=4)
for (row_index in 1:num_row) {
  sample_df <<- getSamplesInSaccade(row_index)
  if (sum(is.na(sample_df$gxR)) == 0) {
    resultsTable[row_index,] <- measureAll()
  }
}