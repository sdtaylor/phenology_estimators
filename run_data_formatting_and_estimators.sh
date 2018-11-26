#!/bin/bash

rm derived_data/*csv

Rscript initial_data_formatting_population.R
Rscript run_estimators_population.R

Rscript initial_data_formatting_individual.R
Rscript run_estimators_individual.R
