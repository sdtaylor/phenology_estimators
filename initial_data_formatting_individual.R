library(tidyverse)
library(lubridate)
library(testthat)

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
# Get the true dates of onset and end of flowering

# The absolute first observations of a flower
onset_flower = doy_data %>%
  filter(flowering == 1) %>%
  group_by(year, plant_id) %>%
  top_n(1, -doy) %>%
  ungroup() %>%
  select(year, plant_id, onset = doy)

end_flower = doy_data %>%
  filter(flowering == 1) %>%
  group_by(year, plant_id) %>%
  top_n(1, doy) %>%
  ungroup() %>%
  select(year, plant_id, end = doy)

flowering_true_dates = onset_flower %>%
  left_join(end_flower, by=c('year','plant_id')) 

# No flowering end date should be prior to flowering onset dat
end_flowering_minus_onset_flowering = with(flowering_true_dates, end-onset)
expect_equal(sum(end_flowering_minus_onset_flowering<=0), 0)

write_csv(flowering_true_dates, individual_true_flowering_dates_file)
#####################################################################
#####################################################################
# Now setup random sampling for input into the estimators

individal_plants_to_keep = doy_data %>%
  group_by(plant_id, year) %>%
  summarise(num_flowering_days = sum(flowering)) %>%
  ungroup() %>% 
  filter(num_flowering_days > minimum_flowering_days_for_individuals) %>%
  pull(plant_id)

doy_data = doy_data %>%
  filter(plant_id %in% individal_plants_to_keep)

flowering_data_for_estimators = data.frame()

for(this_sample_size in individual_sample_sizes){
  for(this_percent_yes in percent_yes){
    for(bootstrap_i in 1:num_bootstraps){
      for(this_year in unique(doy_data$year)){
          
        unique_plants_this_year = doy_data %>%
          filter(year == this_year) %>%
          pull(plant_id) %>%
          unique()
        
        for(this_plant_id in unique_plants_this_year){
          individal_plant_data = doy_data %>%
            filter(year == this_year, plant_id == this_plant_id)
  
          # Get random samples of flowering 'yes' and flowering 'no'
          total_yes_obs = ceiling(this_sample_size * this_percent_yes)
          total_no_obs  = this_sample_size - total_yes_obs
          
          flowering_yes = individal_plant_data %>%
            filter(flowering == 1) %>%
            sample_n(size = total_yes_obs, replace = FALSE)
          flowering_no  = individal_plant_data %>%
            filter(flowering == 0) %>%
            sample_n(size = total_no_obs, replace = FALSE)
          
          # Label it with global parameters and package into the
          # the final data.frame.
          both = flowering_yes %>%
            bind_rows(flowering_no) %>%
            mutate(sample_size = this_sample_size,
                   percent_yes = this_percent_yes,
                   bootstrap_i = bootstrap_i,
                   plant_id    = this_plant_id)
          
          flowering_data_for_estimators = flowering_data_for_estimators %>%
            bind_rows(both)
       }
      }
    }
  }
}

write_csv(flowering_data_for_estimators, individual_data_for_estimators_file)


