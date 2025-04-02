library(dlm)
library(dplyr)
df <- read.csv('powerConsumption.csv', header = TRUE)
str(df)
nrow(df) / 6

### pre-processing ###
df <- df %>%
  mutate()

### single time series ###
