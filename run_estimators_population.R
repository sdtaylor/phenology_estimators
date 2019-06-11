library(tidyverse)
library(progress)

source('config.R')
source('estimators.R')

flowering_data = read_csv(population_data_for_estimators_file)
##################################################################
# 

all_estimates = data.frame()

# setup progress bar
iteration_count = length(population_sample_sizes) * length(percent_yes) * population_num_bootstraps * length(unique(flowering_data$year))
pb = progress_bar$new(total = iteration_count)

print('Running estimators for population level analysis')

for(this_year in unique(flowering_data$year)){
  for(this_sample_size in population_sample_sizes){
    for(this_percent_yes in percent_yes){
      for(this_bootstrap in 1:population_num_bootstraps){
        
        pb$tick()
        
        data_subset = flowering_data %>%
          filter(year == this_year,
                 sample_size == this_sample_size,
                 percent_yes == this_percent_yes,
                 bootstrap_i == this_bootstrap)
        
        subset_estimates = data.frame()
        
        # Onset estimates
        for(estimator_method_name in population_onset_estimators){
          estimate = population_flowering_estimates(data_subset, 
                                                    estimator_name = estimator_method_name,
                                                    flowering_metric = 'onset')
          subset_estimates = subset_estimates %>%
            bind_rows(data.frame(method = estimator_method_name,
                                 'metric' = 'onset',
                                 'estimate' = estimate))
        }
        
        # end estimates
        for(estimator_method_name in population_end_estimators){
          estimate = population_flowering_estimates(data_subset, 
                                                    estimator_name = estimator_method_name,
                                                    flowering_metric = 'end')
          subset_estimates = subset_estimates %>%
            bind_rows(data.frame(method = estimator_method_name,
                                 'metric' = 'end',
                                 'estimate' = estimate))
        }
        
        # peak estimates
        for(estimator_method_name in population_peak_estimators){
          estimate = population_flowering_estimates(data_subset, 
                                                    estimator_name = estimator_method_name,
                                                    flowering_metric = 'peak')
          subset_estimates = subset_estimates %>%
            bind_rows(data.frame(method = estimator_method_name,
                                 'metric' = 'peak',
                                 'estimate' = estimate))
        }
        
        # GAM estimates. These are run separetly from the others since gam models the entire phenology jointly
        for(threshold in gam_thresholds){
          subset_estimates = subset_estimates %>%
            bind_rows(data.frame(method = paste0('gam',threshold*100),
                                 metric = c('onset','peak','end'),
                                 estimate = gam_estimate(data_subset, metric='all', probaility_threshold = threshold)))
          
        }
        
        subset_estimates$year = this_year
        subset_estimates$sample_size = this_sample_size
        subset_estimates$percent_yes = this_percent_yes
        subset_estimates$bootstrap_i = this_bootstrap
        
        all_estimates = all_estimates %>%
          bind_rows(subset_estimates)
      }
    }
  }
}

write_csv(all_estimates, population_estimator_output_file)
