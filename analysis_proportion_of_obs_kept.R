library(tidyverse)

source('config.R')

###########################
# For population estimates
###########################
population_estimates = read_csv(population_estimator_output_file) 

population_proportion_of_samples_kept = population_estimates %>%
  group_by(method, metric, sample_size, percent_yes) %>%
  summarise(n=n(), non_na = sum(!is.na(estimate))) %>%
  ungroup() %>%
  mutate(percent_kept = round(non_na / (population_num_bootstraps*11), 2)) %>%
  complete(method, metric, sample_size, percent_yes)

###################
# Define positioning for the labels on top of the bar graph
label_y_pos = tribble(
  ~metric, ~label_y,
  'end',0.3,
  'onset',0.5,
  'peak',0.7
)

population_proportion_of_samples_kept = population_proportion_of_samples_kept %>%
  left_join(label_y_pos, by='metric')

##################
# Nice lables for the methods
population_proportion_of_samples_kept$method = as.factor(population_proportion_of_samples_kept$method)
population_proportion_of_samples_kept$method = forcats::fct_recode(population_proportion_of_samples_kept$method, 'First Observed'='first_observed',
                                               'Mean Flowering' = 'mean_flowering',
                                               'Survival Curve' = 'survival_curve_median',
                                               'Mean Midway' = 'mean_midway',
                                               'Mean Midway 7-Day' = "mean_midway_7day",
                                               'Logistic' = 'logistic',
                                               'GAM' = 'gam',
                                               'Weibull Curve' = 'pearse')

pop_percent_kept = ggplot(population_proportion_of_samples_kept, aes(x=method, y=percent_kept, fill=metric)) + 
  geom_col(position = position_dodge(width=0.7), width=0.5) + 
  geom_label(aes(label=percent_kept, group=metric, y=label_y, fill=metric),fontface='bold', size=2.5, color='white', position = position_dodge(width=0.7)) + 
  scale_fill_manual(values=c('#E69F00', '#009E73', '#CC79A7')) +
  scale_color_manual(values=c('#E69F00', '#009E73', '#CC79A7')) +
  coord_flip() + 
  facet_wrap(sample_size~percent_yes, labeller = label_both) +
  theme_bw() +
  labs(x='Method',y='Percent of Population Level Estimates Kept', fill='Metric')

ggsave(filename = 'manuscript/population_percent_kept.png', plot = pop_percent_kept, dpi = 500, height = 20, width = 22, units = 'cm')

###########################
# For individual estimates
###########################
individual_estimates = read_csv(individual_estimator_output_file) %>%
  filter(metric!='peak')

individual_proportion_of_samples_kept = individual_estimates %>%
  group_by(method, metric, sample_size, percent_yes) %>%
  summarise(n=n(), non_na = sum(!is.na(estimate))) %>%
  ungroup() %>%
  mutate(percent_kept = round(non_na / 2420, 2))

###################
# Define positioning for the labels on top of the bar graph
label_y_pos = tribble(
  ~metric, ~label_y,
  'end',0.3,
  'onset',0.5,
  'peak',0.7
)

individual_proportion_of_samples_kept = individual_proportion_of_samples_kept %>%
  left_join(label_y_pos, by='metric')

##################
# Nice lables for the methods
individual_proportion_of_samples_kept$method = as.factor(individual_proportion_of_samples_kept$method)
individual_proportion_of_samples_kept$method = forcats::fct_recode(individual_proportion_of_samples_kept$method, 'First Observed'='first_observed',
                                                                   'Midway' = 'midway',
                                                                   'Midway 7-Day' = "midway_7day",
                                                                   'Logistic' = 'logistic',
                                                                   'GAM' = 'gam',
                                                                   'Weibull Curve' = 'pearse')

#################
ind_percent_kep = ggplot(individual_proportion_of_samples_kept, aes(x=method, y=percent_kept, fill=metric)) + 
  geom_col(position = position_dodge(width=0.7), width=0.3) + 
  geom_label(aes(label=percent_kept, group=metric, y=label_y, fill=metric),fontface='bold', size=2.5, color='white', position = position_dodge(width=0.7)) + 
  scale_fill_manual(values=c('#E69F00', '#009E73', '#CC79A7')) +
  scale_color_manual(values=c('#E69F00', '#009E73', '#CC79A7')) +
  coord_flip() + 
  facet_wrap(sample_size~percent_yes, labeller = label_both) +
  theme_bw() +
  labs(x='Method',y='Percent of Individual Level Estimates Kept', fill='Metric')

ggsave(filename = 'manuscript/individual_percent_kept.png', plot = ind_percent_kep, dpi = 500, height = 20, width = 22, units = 'cm')

