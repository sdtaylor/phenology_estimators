library(tidyverse)

source('config.R')

###########################
# For population estimates
###########################
population_estimates = read_csv(population_estimator_output_file) 

# Use only a single threshold for gam/logistic since this result is the same
# for all of them. 
population_estimates = population_estimates %>%
  mutate(method = str_replace(method, 'mean_midway_7day','mean_midway_seven_day')) %>% # get the number outa this so the regex works.
  tidyr::extract(method, c('method', 'threshold'), regex="(\\D+)(\\d+|$)", convert=T) %>%
  filter((!method %in% c('gam','logistic')) | (method %in% c('gam','logistic') & threshold==50)) %>%
  select(-threshold)

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
population_proportion_of_samples_kept$method = forcats::fct_recode(population_proportion_of_samples_kept$method, 'First/Last Observed'='first_observed',
                                               'Mean Flowering' = 'mean_flowering',
                                               'Survival Curve' = 'survival_curve_median',
                                               'Mean Midway' = 'mean_midway',
                                               'Mean Midway 7-Day' = "mean_midway_seven_day",
                                               'Logistic' = 'logistic',
                                               'GAM' = 'gam',
                                               'Weibull' = 'pearse')

population_proportion_of_samples_kept = population_proportion_of_samples_kept %>%
  mutate(sample_size_display = paste('Sample Size',sample_size, sep = ' : '),
         percent_yes_display = paste0('Presence Percent : ',percent_yes*100,'%'))
population_proportion_of_samples_kept$sample_size_display = forcats::fct_reorder(population_proportion_of_samples_kept$sample_size_display, 
                                                                                 population_proportion_of_samples_kept$sample_size)

pop_percent_kept = ggplot(population_proportion_of_samples_kept, aes(x=method, y=percent_kept, fill=metric)) + 
  geom_col(position = position_dodge(width=0.7), width=0.5) + 
  geom_label(aes(label=percent_kept, group=metric, y=label_y, fill=metric),fontface='bold', size=2.5, color='white', position = position_dodge(width=0.7)) + 
  scale_fill_manual(values=c('#E69F00', '#009E73', '#CC79A7')) +
  scale_color_manual(values=c('#E69F00', '#009E73', '#CC79A7')) +
  coord_flip() + 
  facet_wrap(sample_size_display~percent_yes_display) +
  theme_bw() +
  labs(x='Method',y='Percent of Population Level Estimates Kept', fill='Metric')

ggsave(filename = 'manuscript/figs/fig_S1_population_percent_kept.png', plot = pop_percent_kept, dpi = 500, height = 20, width = 22, units = 'cm')

###########################
# For individual estimates
###########################
individual_estimates = read_csv(individual_estimator_output_file) %>%
  filter(metric!='peak')

# Use only a single threshold for gam/logistic since this result is the same
# for all of them. 
individual_estimates = individual_estimates %>%
  mutate(method = str_replace(method, 'midway_7day','midway_seven_day')) %>% # get the number outa this so the regex works.
  tidyr::extract(method, c('method', 'threshold'), regex="(\\D+)(\\d+|$)", convert=T) %>%
  filter((!method %in% c('gam','logistic')) | (method %in% c('gam','logistic') & threshold==50)) %>%
  select(-threshold)

individual_proportion_of_samples_kept = individual_estimates %>%
  group_by(method, metric, sample_size, percent_yes) %>%
  summarise(n=n(), non_na = sum(!is.na(estimate))) %>%
  ungroup() %>%
  mutate(percent_kept = round(non_na / 4840, 2))

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
individual_proportion_of_samples_kept$method = forcats::fct_recode(individual_proportion_of_samples_kept$method, 'First/Last Observed'='first_observed',
                                                                   'Midway' = 'midway',
                                                                   'Midway 7-Day' = "midway_seven_day",
                                                                   'Logistic' = 'logistic',
                                                                   'GAM' = 'gam',
                                                                   'Weibull' = 'pearse')

individual_proportion_of_samples_kept = individual_proportion_of_samples_kept %>%
  mutate(sample_size_display = paste('Sample Size',sample_size, sep = ' : '),
         percent_yes_display = paste0('Presence Percent : ',percent_yes*100,'%'))
individual_proportion_of_samples_kept$sample_size_display = forcats::fct_reorder(individual_proportion_of_samples_kept$sample_size_display, 
                                                                                 individual_proportion_of_samples_kept$sample_size)


#################
ind_percent_kep = ggplot(individual_proportion_of_samples_kept, aes(x=method, y=percent_kept, fill=metric)) + 
  geom_col(position = position_dodge(width=0.7), width=0.3) + 
  geom_label(aes(label=percent_kept, group=metric, y=label_y, fill=metric),fontface='bold', size=2.5, color='white', position = position_dodge(width=0.7)) + 
  scale_fill_manual(values=c('#E69F00', '#009E73', '#CC79A7')) +
  scale_color_manual(values=c('#E69F00', '#009E73', '#CC79A7')) +
  coord_flip() + 
  facet_wrap(sample_size_display~percent_yes_display) +
  theme_bw() +
  labs(x='Method',y='Percent of Individual Level Estimates Kept', fill='Metric')

ggsave(filename = 'manuscript/figs/fig_S2_individual_percent_kept.png', plot = ind_percent_kep, dpi = 500, height = 20, width = 22, units = 'cm')

