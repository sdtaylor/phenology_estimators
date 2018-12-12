library(tidyverse)
library(lubridate)

source('config.R')

################################################################
# This script takes the raw data from Waananen et al. 2018 and converts
# it to the format used in this analysis
################################################################


original_data = read_csv('data/2005_2015_Echinacea_1996cohort_phenology_data.csv') %>%
  mutate(doy_start = yday(start),
         doy_end = yday(end)) %>%
  select(plant_id = cgHdId,
         year = phenYear,
         doy_start, doy_end)

# create plant_id's for the year 2010, all of which are missing from the original dataset
# This analysis does not track individual plants across years, so making up ID's should be fine
num_na_plant_ids = sum(is.na(original_data$plant_id))
original_data$plant_id[is.na(original_data$plant_id)] = 1:num_na_plant_ids

###########
# Convert to daily yes/no for flowers for every year and individual
############

#######
# convert entries of the start and end doy for each plant_id/year
# to an entry for every doy of flowering for each plant_id/year
expand_doy = function(df){
  data.frame(year = df$year,
             doy  = seq(df$doy_start, df$doy_end),
             plant_id = df$plant_id)
}

doy_data = original_data %>%
  rowwise() %>%
  do(expand_doy(.))

doy_data$flowering = 1

########
# Put in 0's for all other days each plant wasn't observed to be in flower
all_possible_days = data.frame(date = seq(ymd('2005-01-01'), ymd('2015-12-31'), by='1 day')) %>%
  mutate(year = year(date),
         doy  = yday(date)) %>%
  as_tibble()

# Also Ensure 0's are only put in for plant_id's which were observed to be in flower for any specific year.
# ie. if plant_id 2008144 was only observed in flower in 2008, and 2010. don't create observerations of it 
# *not* in flower for any othe ryear.
plant_id_years = original_data %>%
  select(plant_id, year) %>%
  distinct()

all_possible_days = all_possible_days %>%
  left_join(plant_id_years, by='year')

doy_data = all_possible_days %>%
  left_join(doy_data, by=c('plant_id','doy','year')) %>%
  mutate(flowering = ifelse(is.na(flowering), 0, flowering))

flower_counts = doy_data %>%
  group_by(year, doy) %>%
  summarise(num_flowers = sum(flowering)) %>%
  ungroup() %>%
  filter(num_flowers >0) 

###########################################

select_years = c(2005,2010,2014)

###########################################
# make  the plant id a sequence from 1:n so it can
# be on the same  y scale as the total number of flowers
individual_data = original_data %>%
  group_by(year) %>%
  arrange(doy_start) %>%
  mutate(plant_id = 1:n()) %>%
  ungroup() %>%
  filter(year %in% select_years)

##############################################################################

ggplot(filter(flower_counts, year %in% select_years), aes(x=doy, y=num_flowers)) +
  geom_segment(data = individual_data, aes(x=doy_start, y=plant_id, xend=doy_end, yend=plant_id), size=0.8, color='grey40') + 
  geom_point(color='black', shape=1, size=4, stroke=2) + 
  geom_line(size=1) + 
  facet_wrap(~year) +
  theme_bw() +
  labs(y='Number of Flowering Individuals', x='Julian Day')


