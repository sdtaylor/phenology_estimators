
# Configuration for various analysis choices
# In the initial_data_formatting script, these
# values will be used to build the dataset used by
# the estimators. 
# For each year this will be x number of samples, consiting of x percent of observations flowering=1,
# and repated x number of times (bootstraps) to get variation. 

# In the estimator script, each estimator will be applied to each permutation.

population_num_bootstraps = 1000
individual_num_bootstraps = 20

percent_yes   = c(0.25, 0.5, 0.75)

individual_sample_sizes  = c(10,15,20)
population_sample_sizes  = c(10,50,100)


minimum_flowering_days_for_individuals = 20

individual_data_for_estimators_file = 'derived_data/individual_flowering_data_for_estimators.csv'
individual_true_flowering_dates_file = 'derived_data/individual_true_flowering_dates.csv'
individual_estimator_output_file = 'derived_data/individual_results_from_estimators.csv'

population_data_for_estimators_file = 'derived_data/population_flowering_data_for_estimators.csv'
population_true_flowering_dates_file = 'derived_data/population_true_flowering_dates.csv'
population_estimator_output_file = 'derived_data/population_results_from_estimators.csv'

#################
population_gam_logistic_threshold_results_file = 'derived_data/population_gam_logistic_threshold_results.csv'

#################
population_onset_estimators = c('first_observed','mean_midway','mean_midway_7day','pearse',
                                'logistic5','logistic25','logistic50','logistic75','logistic95'
                              )
population_end_estimators =   c('first_observed','mean_midway','mean_midway_7day','pearse',
                                'logistic5','logistic25','logistic50','logistic75','logistic95'
                              )
population_peak_estimators  = c('survival_curve_median','mean_flowering')

individual_onset_estimators = c('first_observed','midway','midway_7day','pearse',
                                'logistic5','logistic25','logistic50','logistic75','logistic95'
                              )
individual_end_estimators =   c('first_observed','midway','midway_7day','pearse',
                                'logistic5','logistic25','logistic50','logistic75','logistic95'
                              )

# GAM models are done slightly differently, so only the potential thresholds
# for it are specified here.
gam_thresholds = c(0.05,0.25,0.5,0.75,0.95)
