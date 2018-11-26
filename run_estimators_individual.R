library(tidyverse)

source('config.R')
source('estimators.R')

flowering_data = read_csv(individual_data_for_estimators_file)
##################################################################
# 

all_estimates = data.frame()

for(this_year in unique(flowering_data$year)){
  unique_plants_this_year = flowering_data %>%
    filter(year == this_year) %>%
    pull(plant_id) %>%
    unique()
  
  for(this_sample_size in individual_sample_sizes){
    for(this_percent_yes in percent_yes){
      for(this_bootstrap in 1:num_bootstraps){
        for(this_plant_id in unique_plants_this_year){
        
        data_subset = flowering_data %>%
          filter(year == this_year,
                 sample_size == this_sample_size,
                 percent_yes == this_percent_yes,
                 bootstrap_i == this_bootstrap,
                 plant_id    == this_plant_id)
        
        subset_estimates = data.frame()
        
        for(estimator_method_name in individual_onset_estimators){
          estimate = individual_flowering_estimates(data_subset, 
                                                    estimator_name = estimator_method_name,
                                                    flowering_metric = 'onset')
          subset_estimates = subset_estimates %>%
            bind_rows(data.frame(method = estimator_method_name,
                                 'metric' = 'onset',
                                 'estimate' = estimate))
        }
        for(estimator_method_name in individual_end_estimators){
          estimate = individual_flowering_estimates(data_subset, 
                                                    estimator_name = estimator_method_name,
                                                    flowering_metric = 'end')
          subset_estimates = subset_estimates %>%
            bind_rows(data.frame(method = estimator_method_name,
                                 'metric' = 'end',
                                 'estimate' = estimate))
        }
        
        # GAM estimates. These are run separetly from the others since gam models the entire phenology jointly
        subset_estimates = subset_estimates %>%
          bind_rows(data.frame(method = 'gam',
                               metric = c('onset','peak','end'),
                               estimate = gam_estimate(data_subset, metric='all')))
        
        subset_estimates$year = this_year
        subset_estimates$sample_size = this_sample_size
        subset_estimates$percent_yes = this_percent_yes
        subset_estimates$bootstrap_i = this_bootstrap
        subset_estimates$plant_id    = this_plant_id
        
        all_estimates = all_estimates %>%
          bind_rows(subset_estimates)
      }
    }
  }
  }
}

write_csv(all_estimates, individual_estimator_output_file)
