library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)
library(mapproj)
library(DT)
library(ggthemes)

# load all of bird data except breed season
birdDf <- data.frame()
bird_files <- list.files('bird_data')
for (bird_file in bird_files) {
  if (bird_file == 'breed_season.csv') {
    breedSeasonDf <- read.csv('bird_data/breed_season.csv')
  } else {
    tempDf <- read.csv(paste('bird_data/', bird_file, sep=""))
    birdDf <- rbind(birdDf, tempDf)
    rm(tempDf)
  }
}

# preprocessing
birdDf$date <- as.Date(birdDf$date, format='%Y-%m-%d')
birdDf <- left_join(birdDf, breedSeasonDf, by=c('common.name'='common_name'))
birdDf <- birdDf %>% mutate(., breed=ifelse(as.integer(format(birdDf$date, "%m")) >= month_from & as.integer(format(birdDf$date, "%m")) <= month_to, 1, 0)) %>%
  separate(location, c('region', 'subregion'), ',') %>%
  select(c('common.name', 'region', 'subregion', 'date', 'breed'))

# load map data
state_map <- map_data('state')
county_map <- map_data('county')
