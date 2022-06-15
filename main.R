library(eyelinkReader)

source("./measures.R")

gaze <- read_edf('data/jim.edf',import_samples=TRUE)

applyToAll <- function(metric=1) {
  measures <- list('measureAll', 'measureAngle', 'measureOrtho', 'measureArea')
  
  num_row <- nrow(gaze$saccades)
  vec <- rep(NA, num_row)
  
  for (row_index in 1:num_row) {
    vec[row_index] <- do.call(measures[[metric]], list(row_index))
  }
  vec
}
