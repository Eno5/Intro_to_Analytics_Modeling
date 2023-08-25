# Q12.2
library(FrF2)
library(dplyr)
set.seed(101)

houses <- FrF2(16, 10, factor.names = c(
  'Large Yard', 'Pool', 'Solar Roof', 'Long Driveway', 'Multi-Car Garage',
  'Walk-In Closets', 'Full Bar', 'Gazebo', 'Elevator', 'Basement'),
  default.levels = c('Yes', 'No')) %>% as_tibble()