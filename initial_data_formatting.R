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

#####################################################################
#####################################################################
# Get the true dates of onset, peak, and 25% of peak

flower_counts = doy_data %>%
  group_by(year, doy) %>%
  summarise(num_flowers = sum(flowering)) %>%
  ungroup() %>%
  filter(num_flowers >0) 

# doy with the highest number of open flowers.
# Taking only the first occurance if peak flower lasts >1 day
peak_flower = flower_counts %>%
  group_by(year) %>%
  filter(num_flowers == max(num_flowers)) %>%
  top_n(1, -doy) %>%
  select(year, peak_flower = doy)

# The absolute first observations of a flower
onset_flower = flower_counts %>%
  group_by(year) %>%
  top_n(1, -doy) %>%
  ungroup() %>%
  select(year, first_flower = doy)

# The doy when flowers are ~25% of the peak amount
flower_percentiles = flower_counts %>%
  group_by(year) %>%
  mutate(peak_flower = max(num_flowers)) %>%
  mutate(flower_percent = num_flowers / peak_flower) %>%
  filter(flower_percent >= 0.25) %>%
  top_n(1, -doy) %>%
  ungroup() %>%
  select(year, flowering_25_of_peak = doy)
  
flowering_true_dates = onset_flower %>%
  left_join(flower_percentiles, by='year') %>%
  left_join(peak_flower, by='year')

write_csv(flowering_true_dates, true_flowering_dates_file)
#####################################################################
#####################################################################
# Now setup random sampling for input into the estimators

flowering_data_for_estimators = data.frame()

for(this_year in unique(doy_data$year)){
  for(this_sample_size in sample_sizes){
    for(this_percent_yes in percent_yes){
      for(bootstrap_i in 1:num_bootstraps){
        
        year_data = doy_data %>%
          filter(year == this_year) %>%
          select(year, doy, flowering)
        
        # Drop all observations past the very last flowering date for the year,
        # since all estimators are designed to work with data leading up to, and including, flowering.
        last_flowering_doy = year_data %>%
          filter(flowering==1) %>%
          pull(doy) %>%
          max()
        year_data = year_data %>%
          filter(doy <= last_flowering_doy)
        
        # Get random samples of flowering 'yes' and flowering 'no'
        total_yes_obs = ceiling(this_sample_size * this_percent_yes)
        total_no_obs  = this_sample_size - total_yes_obs
        
        flowering_yes = year_data %>%
          filter(flowering == 1) %>%
          sample_n(size = total_yes_obs, replace = FALSE)
        flowering_no  = year_data %>%
          filter(flowering == 0) %>%
          sample_n(size = total_no_obs, replace = FALSE)
        
        # Label it with global parameters and package into the
        # the final data.frame.
        both = flowering_yes %>%
          bind_rows(flowering_no) %>%
          mutate(sample_size = this_sample_size,
                 percent_yes = this_percent_yes,
                 bootstrap_i = bootstrap_i)
        
        flowering_data_for_estimators = flowering_data_for_estimators %>%
          bind_rows(both)
        
        
      }
    }
  }
}

write_csv(flowering_data_for_estimators, data_for_estimators_file)


