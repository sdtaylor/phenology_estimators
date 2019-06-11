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
#Some flowering data to use in testing
# sample_data = read_csv('derived_data/population_flowering_data_for_estimators.csv') %>%
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

##################################################
# Helper function to drop flowering=0 observations.
# Drops leading (leading up to flower) observations when
# estimating the end of flowering. Drops trailign when
# estimating the onset of flowering
drop_zeros = function(fl, type_to_drop='leading'){
  flowering_days = fl %>%
    filter(flowering==1) %>%
    pull(doy)
  
  if(type_to_drop == 'leading'){
    fl = fl %>%
      filter(doy >= min(flowering_days))
  } else if(type_to_drop == 'trailing'){
    fl = fl %>%
      filter(doy <= max(flowering_days))
  } else {
    stop('Unknown type to drop: ',type_to_drop)
  }
  return(fl)
}

###################################################
# GAM model
#
# This draws a logistic smoother over the entire year
# peak: the maximum probability of flower
# onset: first doy prior to peak where flowering is >= threshold
# end: last doy after peak where flowering is <= threshold
####################################################

gam_estimate = function(fl, metric='onset', probaility_threshold){
  gam_model = tryCatch({mgcv::gam(flowering ~ s(doy), family=binomial, data=fl)},
                        error = function(x){return(NA)})

  if(is.na(gam_model)){
    return(NA)
    }
  
  all_doys = data.frame(doy = 1:365)
  all_doys$flowering_probability = predict(gam_model, newdata = all_doys, type = 'response')
  peak_doy = all_doys$doy[which.max(all_doys$flowering_probability)]
  onset_doy = all_doys %>%
    filter(doy <= peak_doy, flowering_probability >= probaility_threshold) %>%
    pull(doy) %>%
    min()
  end_doy = all_doys %>%
    filter(doy >= peak_doy, flowering_probability <= probaility_threshold) %>%
    pull(doy) %>%
    min()
  
  if(metric=='peak'){
    return(peak_doy)
  } else if(metric=='onset'){
    return(onset_doy)
  } else if(metric=='end'){
    return(end_doy)
  } else if(metric=='all'){
    #return(list('onset' = onset_doy, 'peak' = peak_doy, 'end' = end_doy))
    return(c(onset_doy, peak_doy, end_doy))
  } else if(metric=='all_doys'){
    return(all_doys)
  } else {
    stop('Unknown metric type: ',metric)
  }
}

###################################################
# First observed Flowering Date
#
# The first/last time a flower is seen is the FFD/last flowering day
####################################################

first_observed = function(fl){
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
# Midway Method for an individual
#
# The  mideway point between first "yes" and most
# prior "no". Rounded down to the doy when the midway is a fraction.
####################################################

midway_individual = function(fl, prior_no_threshold=Inf){
  # Ensure only one individual is plant being passed.
  expect_length(unique(fl$plant_id), 1)
  
  first_yes = fl %>%
    filter(flowering == 1) %>%
    pull(doy) %>%
    min()
  
  most_prior_no = fl %>%
    filter(flowering == 0, doy<=first_yes) %>%
    pull(doy) %>%
    max()
  
  if(first_yes - most_prior_no > prior_no_threshold){
    return(NA)
  }
  
  midway_point = first_yes - ((first_yes - most_prior_no)/2)
  
  # Sometimes the midway point is not resolved because the random sample
  # did not pick up any 0 observations prior to the first 1 observation.
  # In this case just drop it by returning NA
  if(is.infinite(midway_point)){
    return(NA)
  } else {
    return(midway_point)
  }
}

###################################################
# Midway Method for a population
#
# The  mideway point between first "yes" and most
# prior "no" for each individual, then get the mean.
# Optionally enforce a threshold in the days since
# the most prior "no" for each individual
####################################################

midway_population = function(fl, population_prior_no_threshold=Inf){
  
  individual_onsets = c()
  for(plant in unique(fl$plant_id)){
    individual_data = fl %>%
      filter(plant_id==plant)
    individual_onsets = c(individual_onsets, midway_individual(individual_data, prior_no_threshold = population_prior_no_threshold))
  }
  
  return(mean(individual_onsets, na.rm=T))
}

###################################################
# Logistic Regression
#
# Do a logistic regression of flowering ~ doy, and use the 
# first doy which has a 50% probability of flowering (will also be the inflection point)
####################################################

logistic_method = function(fl, probability_threshold, metric, return_all_doys = FALSE){
  logistic_model = glm(flowering ~ doy, family = 'binomial', data=fl)
  all_doys = data.frame(doy = 1:365)
  all_doys$flowering_probability = predict(logistic_model, newdata = all_doys, type = 'response')
  
  if(metric=='onset'){
    estimate = all_doys %>%
      filter(flowering_probability >= probability_threshold) %>%
      pull(doy) %>%
      min()
  } else if(metric == 'end'){
    estimate = all_doys %>%
      filter(flowering_probability <= probability_threshold) %>%
      pull(doy) %>%
      min()
  }

  
  if(return_all_doys){
    return(all_doys)
  } else {
    return(estimate)
  }
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
  as.numeric(weib.limit(flowering_yes$doy)[1])
}

###################################################
# Survival Curve
#
# Uses a Kaplan-Meier Survival curve to estimate
# the mean date of "death", in this case death = flowering
####################################################
library(survival)
survival_curve_method = function(fl, type = 'median'){
  survival_model = survival::survfit(Surv(time = doy, event = flowering, type='right') ~ 1, data = fl)
  model_estimates = summary(survival_model)$table
  model_mean = model_estimates[5]
  model_median = model_estimates[7]
  # Make sure the position of the mean and median at 5 and 7 in the summary table doesn't change
  expect_equal(names(model_mean),'*rmean')
  expect_equal(names(model_median), 'median')
  if(type == 'mean'){
    return(as.numeric(model_mean))
  } else if(type == 'median'){
    return(as.numeric(model_median))
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
###################################################
# Wrappers that will actually be called to run the estimators
# These function perform various transformations depending on which
# estimator is being used for onset,end, or peak.
#
# For example, using the weibull curve/pearse method the same code for the onset of 
# flowering can be used to calculate end of flowering if you just transform all doy
# values to negative, and then the result back to postiive. 
###################################################
###################################################

population_flowering_estimates = function(fl, estimator_name, flowering_metric){
  #####
  #Onset estimators
  if(flowering_metric == 'onset'){
    fl = drop_zeros(fl, type_to_drop = 'trailing')
    
    if(estimator_name=='first_observed'){
      return(first_observed(fl))
    } else if(estimator_name=='mean_midway'){
      return(midway_population(fl))
    } else if(estimator_name=='mean_midway_7day'){
      return(midway_population(fl, population_prior_no_threshold=7))
      
    } else if(estimator_name=='logistic5'){
      return(logistic_method(fl, probability_threshold = 0.05, metric = 'onset'))
      
    } else if(estimator_name=='logistic25'){
      return(logistic_method(fl, probability_threshold = 0.25, metric = 'onset'))
      
    } else if(estimator_name=='logistic50'){
      return(logistic_method(fl, probability_threshold = 0.5, metric = 'onset'))
      
    } else if(estimator_name=='logistic75'){
      return(logistic_method(fl, probability_threshold = 0.75, metric = 'onset'))
      
    } else if(estimator_name=='logistic95'){
      return(logistic_method(fl, probability_threshold = 0.95, metric = 'onset'))
      
    } else if(estimator_name=='pearse'){
      return(pearse_method(fl))
    } else {
      stop('Unknown estimator name for onset ',estimator_name)
    }
    
    #####
    #End estimators
  } else if(flowering_metric == 'end'){
    fl = drop_zeros(fl, type_to_drop = 'leading')
    
    if(estimator_name=='first_observed'){
      fl$doy = fl$doy * -1
      return(first_observed(fl)*-1)
      
    } else if(estimator_name=='mean_midway'){
      fl$doy = fl$doy * -1
      return(midway_population(fl) * -1)
    } else if(estimator_name=='mean_midway_7day'){
      fl$doy = fl$doy * -1
      return(midway_population(fl, population_prior_no_threshold=7) * -1)
      
    } else if(estimator_name=='logistic5'){
      return(logistic_method(fl, probability_threshold = 0.05, metric = 'end'))
      
    } else if(estimator_name=='logistic25'){
      return(logistic_method(fl, probability_threshold = 0.25, metric = 'end'))
      
    } else if(estimator_name=='logistic50'){
      return(logistic_method(fl, probability_threshold = 0.5, metric = 'end'))
      
    } else if(estimator_name=='logistic75'){
      return(logistic_method(fl, probability_threshold = 0.75, metric = 'end'))
      
    } else if(estimator_name=='logistic95'){
      return(logistic_method(fl, probability_threshold = 0.95, metric = 'end'))
      
    } else if(estimator_name=='pearse'){
      #transpose flowering dates to negative
      #to estimate the end of flowering
      #using weibull curve/pearse method
      fl$doy = fl$doy * -1
      return(pearse_method(fl) * -1)
    } else {
      stop('Unknown estimator name for end of flowering ',estimator_name)
    }
    
    #####
    #Peak estimators
  } else if(flowering_metric == 'peak'){
    
    if(estimator_name=='survival_curve_median'){
      fl = drop_zeros(fl, type_to_drop = 'trailing')
      return(survival_curve_median(fl))
    } else if(estimator_name=='mean_flowering'){
      return(mean_flowering(fl))
    } else {
      stop('Unknown estimator name for peak flowering',estimator_name)
    }
  }
}

####################
individual_flowering_estimates = function(fl, estimator_name, flowering_metric){
  #####
  #Onset estimators
  if(flowering_metric == 'onset'){
    fl = drop_zeros(fl, type_to_drop = 'trailing')
    
    if(estimator_name=='first_observed'){
      return(first_observed(fl))
      
    } else if(estimator_name=='midway'){
      return(midway_individual(fl))
    } else if(estimator_name=='midway_7day'){
      return(midway_individual(fl, prior_no_threshold=7))
      
    } else if(estimator_name=='logistic5'){
      return(logistic_method(fl, probability_threshold = 0.05))
      
    } else if(estimator_name=='logistic25'){
      return(logistic_method(fl, probability_threshold = 0.25))
      
    } else if(estimator_name=='logistic50'){
      return(logistic_method(fl, probability_threshold = 0.5))
      
    } else if(estimator_name=='logistic75'){
      return(logistic_method(fl, probability_threshold = 0.75))
      
    } else if(estimator_name=='logistic95'){
      return(logistic_method(fl, probability_threshold = 0.95))
      
    } else if(estimator_name=='pearse'){
      return(pearse_method(fl))
    } else {
      stop('Unknown estimator name for onset ',estimator_name)
    }
    
    #####
    #End estimators
  } else if(flowering_metric == 'end'){
    fl = drop_zeros(fl, type_to_drop = 'leading')
    
    if(estimator_name=='first_observed'){
      fl$doy = fl$doy * -1
      return(first_observed(fl)*-1)
      
    } else if(estimator_name=='midway'){
      fl$doy = fl$doy * -1
      return(midway_individual(fl) * -1)
    } else if(estimator_name=='midway_7day'){
      fl$doy = fl$doy * -1
      return(midway_individual(fl, prior_no_threshold=7) * -1)
      
    } else if(estimator_name=='logistic5'){
      #swap the flowering so 0=1 and 1=0
      #for the end of flowering estimate using
      #logistic regression
      fl$flowering = abs(fl$flowering - 1)
      return(logistic_method(fl, probability_threshold = 0.05))
      
    } else if(estimator_name=='logistic25'){
      fl$flowering = abs(fl$flowering - 1)
      return(logistic_method(fl, probability_threshold = 0.25))
      
    } else if(estimator_name=='logistic50'){
      fl$flowering = abs(fl$flowering - 1)
      return(logistic_method(fl, probability_threshold = 0.5))
      
    } else if(estimator_name=='logistic75'){
      fl$flowering = abs(fl$flowering - 1)
      return(logistic_method(fl, probability_threshold = 0.75))
      
    } else if(estimator_name=='logistic95'){
      fl$flowering = abs(fl$flowering - 1)
      return(logistic_method(fl, probability_threshold = 0.95))
      
    } else if(estimator_name=='pearse'){
      #transpose flowering dates to negative
      #to estimate the end of flowering
      #using weibull curve/pearse method
      fl$doy = fl$doy * -1
      return(pearse_method(fl) * -1)
    } else {
      stop('Unknown estimator name for end of flowering ',estimator_name)
    }
  } 
}
