library(tidyverse)

source('config.R')
source('estimators.R')

flowering_data = read_csv(data_for_estimators_file)
##################################################################
# 

all_estimates = data.frame()

for(this_year in unique(flowering_data$year)){
  for(this_sample_size in sample_sizes){
    for(this_percent_yes in percent_yes){
      for(this_bootstrap in 1:num_bootstraps){
        
        data_subset = flowering_data %>%
          filter(year == this_year,
                 sample_size == this_sample_size,
                 percent_yes == this_percent_yes,
                 bootstrap_i == this_bootstrap)
        
        subset_estimates = data.frame()
        
        for(estimator_method_name in names(estimator_list)){
          
          # Estimator function called here
          estimate = estimator_list[estimator_method_name][[1]](data_subset)
          
          subset_estimates = subset_estimates %>%
            bind_rows(data.frame(method = estimator_method_name,
                                 estimate = estimate))
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

write_csv(all_estimates, estimator_output_file)
