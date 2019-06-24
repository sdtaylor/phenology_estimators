# Estimating transition dates from status-based phenological observations: a test of methods

The is the code reposity for the following manuscript:

Taylor SD. 2019. Estimating transition dates from status-based phenology observations: a test of methods. PeerJ Preprints 7:e27629v1 [https://doi.org/10.7287/peerj.preprints.27629v1](https://doi.org/10.7287/peerj.preprints.27629v1)

## File structure 

`data/`:
    This holds the original data from the [Waananen et. al. 2018 study](http://doi.org/10.1086/698657), obtained from [https://doi.org/10.5061/dryad.487db24](https://doi.org/10.5061/dryad.487db24).

`derived_data/`:
    This holds the phenology data prepared for the analysis, and the output from the estimators. Due to size this data is not in the GitHub repo, but is in the repository ([https://doi.org/10.5281/zenodo.3234913](https://doi.org/10.5281/zenodo.3234913))

`manuscript/`:
    The manuscript Rmarkdown files and figures.

`tests/`:
    Tests to ensure the code in the `estimators.R` script are functioning correctly, as well as some small example data to run the tests with.

`estimators.R`
`weibull.R`
    These 2 scripts contain the functions for all estimators. 

`initial_data_formatting_individual.R`
`initial_data_formatting_population.R`
    These 2 scripts generate the phenology data used by the estimators. Taking the raw data from `data` and generating the random Monte Carlo samples with the parameters specified in the text. They generate the following files:
        
    - derived_data/population_flowering_data_for_estimators.csv
    - derived_data/individual_flowering_data_for_estimators.csv
    - derived_data/population_true_flowering_dates.csv
    - derived_data/individual_true_flowering_dates.csv

`run_estimators_individual.R`
`run_estimators_population.R`
    These 2 scripts apply the estimators to the derived data, and write the following result files:

    - population_results_from_estimators.csv
    - individual_results_from_estimators.csv

`analysis_and_figures.R`
`analysis_proportion_of_obs_kept.R`
`analysis_gam_logistic_supplement.R`
    These 3 scripts generate all the figures and statistics in the analysis, including supplements. 

`install_packages.R`
    This script installs the packages used throughout the analysis. 

`config.R`
    The configuration file specifies the parameters used the Monte Carlo analysis as well as specifying all file names.

## Required packages
The following R packages are used in this analysis and can be installed by running the `install_packages.R` script. 

- tidyverse
- mgcv
- survival
- testthat 
- ggridges
- progress

## Running the analysis

To run the analysis from scratch, run the scripts in the following order:

`initial_data_formatting_individual.R`
`initial_data_formatting_population.R`
`run_estimators_individual.R`
`run_estimators_population.R`
`analysis_and_figures.R`
`analysis_proportion_of_obs_kept.R`

This will take from 20-35 hours in total to run depending on the system. To decrease this time set the bootstrap amounts in `config.R` to something smaller, such as 10 for the population and 2 for the individual. The results will not be as precise, but the runtime should be less than 1 hour. 

Alternatively, you can obtain the results from the computationally intensive steps from the Zenodo repository ([https://doi.org/10.5281/zenodo.3234913](https://doi.org/10.5281/zenodo.3234913)) in the `derived_data` folder. With these in place run `analysis_and_figures.R` and `analysis_proportion_of_obs_kept.R` to generate the figures. 
