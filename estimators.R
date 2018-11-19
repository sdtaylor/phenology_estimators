library(tidyverse)
library(testthat)

###################################################
###################################################
# Functions for each of the phenology estimators used in the analysis
###################################################
###################################################


# All estimator functions take a data.frame in the form of:
#
# year   doy flowering sample_size percent_yes bootstrap_i
# <int> <int>     <int>       <int>       <dbl>       <int>
# 2008   211         1         100         0.5           1
# 2008   207         1         100         0.5           1
# 2008   223         1         100         0.5           1
# ..........
#
# Where the data.frame is data from a single year, bootstrap, sample size, etc.
# The function qc_data() will enforce this requirement.

###################################################
# Some flowering data to use in testing
# sample_data = read_csv('derived_data/flowering_data_for_estimators.csv') %>%
#   filter(year == 2008,
#          sample_size == 100,
#          percent_yes == 0.5,
#          bootstrap_i == 1)

#############################################
# Ensure only a single year, sample_size, etc is included in an estimate
qc_data = function(fl){
  expect_length(unique(fl$year), 1)
  expect_length(unique(fl$sample_size), 1)
  expect_length(unique(fl$percent_yes), 1)
  expect_length(unique(fl$bootstrap_i), 1)
}

###################################################
# Naive First Flowering Date
#
# The first time a flower is seen is the FFD
####################################################

naive_ffd = function(fl){
  qc_data(fl)
  fl %>%
    filter(flowering == 1) %>%
    pull(doy) %>%
    min()
}

###################################################
# Mean flowering date
#
# The mean doy of all flowering==1, a very common method
####################################################
mean_flowering = function(fl){
  fl %>%
    filter(flowering == 1) %>%
    pull(doy) %>%
    mean()
}


###################################################
# Midway Method
#
# The  mideway point between first "yes" and most
# prior "no". Rounded down to the doy when the midway is a fraction.
####################################################

midway_method = function(fl){
  first_yes = fl %>%
    filter(flowering == 1) %>%
    pull(doy) %>%
    min()
  
  most_prior_no = fl %>%
    filter(flowering == 0, doy<=first_yes) %>%
    pull(doy) %>%
    max()
  
  midway_point = floor(first_yes - ((first_yes - most_prior_no)/2))
  
  # Sometimes the midway point is not resolved because the random sample
  # did not pick up any 0 observations prior to the first 1 observation.
  # In this case just drop it by returning NA
  if(is.infinite(midway_point)){
    return(NA)
  } else {
    expect_gt(midway_point, 0)
    return(midway_point)
  }
}

###################################################
# Logistic Regression
#
# Do a logistic regression of flowering ~ doy, and use the 
# first doy which has a 50% probability of flowering (will also be the reflection point)
####################################################

logistic_method = function(fl, probability_threshold=0.5){
  logistic_model = glm(flowering ~ doy, family = 'binomial', data=fl)
  all_doys = data.frame(doy = 1:366)
  all_doys$flowering_probability = predict(logistic_model, newdata = all_doys, type = 'response')
  
  all_doys %>%
    filter(flowering_probability >= probability_threshold) %>%
    pull(doy) %>%
    min()
}

###################################################
# Pearse Method
#
# From Pearse et al. 2017 in Nature Ecology & Evolution
# Estimates first flowering from only "yes" observations
# using a weibull curve
####################################################
source('weibull.R')

pearse_method = function(fl){
  flowering_yes = fl %>%
    filter(flowering==1)
  floor(as.numeric(weib.limit(flowering_yes$doy)[1]))
}

###################################################
# Survival Curve
#
# Uses a Kaplan-Meier Survival curve to estimate
# the mean date of "death", in this case death = flowering
####################################################
library(survival)
survival_curve_method = function(fl, type = 'mean'){
  survival_model = survival::survfit(Surv(time = doy, event = flowering, type='right') ~ 1, data = fl)
  model_estimates = summary(survival_model)$table
  model_mean = model_estimates[5]
  model_median = model_estimates[7]
  # Make the position of the mean and median at 5 and 7 doesn't change
  expect_equal(names(model_mean),'*rmean')
  expect_equal(names(model_median), 'median')
  if(type == 'mean'){
    return(floor(as.numeric(model_mean)))
  } else if(type == 'median'){
    return(floor(as.numeric(model_median)))
  } else {
    stop('Unknown survival curve type: ',type)
  }
}

# wrappers for the two estimate types from the survival curve
survival_curve_mean = function(fl){
  survival_curve_method(fl, type='mean')
}

survival_curve_median = function(fl){
  survival_curve_method(fl, type='median')
}

###################################################
# Finally wrap them up in a list to iterate over
estimator_list = list('naive_ffd' = naive_ffd,
                      'mean_flowering' = mean_flowering,
                      'midway_method' = midway_method,
                      'logistic' = logistic_method,
                      'pearse' = pearse_method,
                      'survival_curve_mean' = survival_curve_mean,
                      'survival_curve_median' = survival_curve_median)
