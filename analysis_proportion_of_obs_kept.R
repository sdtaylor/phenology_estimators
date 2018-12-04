library(tidyverse)

source('config.R')

###########################
population_estimates = read_csv(population_estimator_output_file) 

population_proportion_of_samples_kept = population_estimates %>%
  group_by(method, metric, sample_size, percent_yes) %>%
  summarise(n=n(), non_na = sum(!is.na(estimate))) %>%
  ungroup() %>%
  mutate(percent_kept = round(non_na / (population_num_bootstraps*11), 2)) %>%
  complete(method, metric, sample_size, percent_yes)

population_table = population_proportion_of_samples_kept %>%
  select(method, metric, sample_size, percent_yes, percent_kept) %>%
  spread(metric, percent_kept)

ggplot(population_proportion_of_samples_kept, aes(x=method, y=percent_kept, fill=metric)) + 
  geom_col(position = position_dodge(width=0.7), width=0.5) + 
  geom_label(aes(label=percent_kept, group=metric, y=0.5), fill='white', position = position_dodge(width=0.7)) + 
  scale_fill_manual(values=c('#E69F00', '#009E73', '#CC79A7')) +
  coord_flip() + 
  facet_grid(sample_size~percent_yes, labeller = label_both) +
  theme_bw()

###########################
individual_estimates = read_csv(individual_estimator_output_file) %>%
  filter(method!='gam')

individual_proportion_of_samples_kept = individual_estimates %>%
  group_by(method, metric, sample_size, percent_yes) %>%
  summarise(n=n(), non_na = sum(!is.na(estimate))) %>%
  ungroup() %>%
  mutate(percent_kept = round(non_na / 2420, 2))

individual_table = individual_proportion_of_samples_kept %>%
  select(method, metric, sample_size, percent_yes, percent_kept) %>%
  spread(metric, percent_kept)


ggplot(individual_proportion_of_samples_kept, aes(x=method, y=percent_kept, fill=metric)) + 
  geom_col(position = position_dodge(width=0.7), width=0.5) + 
  geom_label(aes(label=percent_kept, group=metric, y=0.5), fill='white', position = position_dodge(width=0.7)) + 
  scale_fill_manual(values=c('#E69F00', '#009E73', '#CC79A7')) +
  coord_flip() + 
  facet_grid(sample_size~percent_yes, labeller = label_both) +
  theme_bw()
