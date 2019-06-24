library(testthat)
library(tidyverse)

source('estimators.R')


##########################################
# Test individual estimators with
# calculations done by hand


#####
individual_test_data = read_csv('tests/individual_test_data.csv')
expect_equal(individual_flowering_estimates(individual_test_data, 'first_observed', 'onset'), 187)
expect_equal(individual_flowering_estimates(individual_test_data, 'midway', 'onset'), 163.5)
expect_equal(individual_flowering_estimates(individual_test_data, 'logistic50', 'onset'), 164)
expect_equal(round(individual_flowering_estimates(individual_test_data, 'pearse', 'onset'),2), 96.58)

expect_equal(individual_flowering_estimates(individual_test_data, 'first_observed', 'end'), 210)
expect_equal(individual_flowering_estimates(individual_test_data, 'midway', 'end'), 238.5)
expect_equal(individual_flowering_estimates(individual_test_data, 'logistic50', 'end'), 239)
expect_equal(round(individual_flowering_estimates(individual_test_data, 'pearse', 'end'),2), 219.28)

######
population_test_data = read_csv('tests/population_test_data.csv')

expect_equal(population_flowering_estimates(population_test_data, 'first_observed', 'onset'), 191)
#expect_equal(population_flowering_estimates(population_test_data, 'mean_midway', 'onset'), NA) # this is not quit working yet
expect_equal(population_flowering_estimates(population_test_data, 'logistic5', 'onset'), 169)
expect_equal(population_flowering_estimates(population_test_data, 'logistic25', 'onset'), 181)
expect_equal(population_flowering_estimates(population_test_data, 'logistic50', 'onset'), 188)
expect_equal(population_flowering_estimates(population_test_data, 'logistic75', 'onset'), 195)
expect_equal(population_flowering_estimates(population_test_data, 'logistic95', 'onset'), 207)
expect_equal(round(population_flowering_estimates(population_test_data, 'pearse', 'onset'),2), 188.61)

expect_equal(population_flowering_estimates(population_test_data, 'first_observed', 'end'), 214)
#expect_equal(population_flowering_estimates(population_test_data, 'mean_midway', 'end'), 239) #
expect_equal(population_flowering_estimates(population_test_data, 'logistic5', 'end'), 276)
expect_equal(population_flowering_estimates(population_test_data, 'logistic25', 'end'), 246)
expect_equal(population_flowering_estimates(population_test_data, 'logistic50', 'end'), 228)
expect_equal(population_flowering_estimates(population_test_data, 'logistic75', 'end'), 210)
expect_equal(population_flowering_estimates(population_test_data, 'logistic95', 'end'), 180)
expect_equal(round(population_flowering_estimates(population_test_data, 'pearse', 'end'),2), 215.39)

expect_equal(population_flowering_estimates(population_test_data, 'survival_curve_median', 'peak'), 204)
expect_equal(population_flowering_estimates(population_test_data, 'mean_flowering', 'peak'), 203.24)
