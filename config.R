
# Configuration for various analysis choices
# In the initial_data_formatting script, these
# values will be used to build the dataset used by
# the estimators. 
# For each year this will be x number of samples, consiting of x percent of observations flowering=1,
# and repated x number of times (bootstraps) to get variation. 

# In the estimator script, each estimator will be applied to each permutation.

num_bootstraps = 10
sample_sizes  = c(10,50,100)
percent_yes   = c(0.25, 0.5, 0.75)


data_for_estimators_file = 'data/flowering_data_for_estimators.csv'
true_flowering_dates_file = 'data/true_flowering_dates.csv'