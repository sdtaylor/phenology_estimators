library(tidyverse)

source('config.R')

#########################################
library(ggridges)

get_plot = function(error_df, metric_to_plot,
                    x_lower_bound=-50, x_upper_bound=50){
  p= 5
  ggplot(filter(error_df, metric==metric_to_plot), aes(x=error, y=method)) + 
    geom_text(aes(y=method, x=-50, label=error_text, size=3.5, hjust=0, vjust=-1)) +
    #geom_density_ridges(fill=NA, size=1.5, aes(color=method)) + 
    geom_vline(xintercept = 0, size=1) + 
    #scale_color_brewer(palette = 'Dark2') + 
    xlim(x_lower_bound,x_upper_bound) + 
    facet_wrap(sample_size_display~percent_yes_display) +
    #theme_light() +
    theme_ridges() + 
    theme(legend.position = 'none',
          axis.title.x = element_text(hjust = 0.5),
          axis.text.y = element_text(size=10),
          axis.text.x = element_text(size=10),
          strip.text = element_text(color='black'),
          strip.background = element_rect(fill='grey90', color='black'),
          text = element_text(family='Helvetica', face='plain', color='black')) + 
    labs(x='Error Distribution (Estimated DOY - True DOY)',
         y='')
  return(p)
}

#############################################
# Error plots for population estiamtes
population_estimates = read_csv(population_estimator_output_file) 

population_true_data = read_csv(population_true_flowering_dates_file) %>%
  gather(metric, actual_doy, -year)

population_errors = population_estimates %>%
  filter(!is.na(estimate), is.finite(estimate)) %>%
  left_join(population_true_data, by=c('year','metric')) %>%
  mutate(error = estimate - actual_doy)

population_errors = population_errors %>%
  group_by(metric, method, sample_size, percent_yes) %>%
  ungroup()

population_errors$method = as.factor(population_errors$method)
population_errors$method = forcats::fct_recode(population_errors$method, 'First Observed'='first_observed',
                                                   'Mean Flowering' = 'mean_flowering',
                                                   'Survival Curve' = 'survival_curve_median',
                                                   'Mean Midway' = 'mean_midway',
                                                   'Mean Midway 7-Day' = "mean_midway_7day",
                                                   'Logistic' = 'logistic',
                                                   'Weibull Curve' = 'pearse')
population_errors = population_errors %>%
  mutate(sample_size_display = paste('Sample Size',sample_size, sep = ' : '),
         percent_yes_display = paste('Percent Yes',percent_yes, sep = ' : '))
population_errors$sample_size_display = forcats::fct_reorder(population_errors$sample_size_display, population_errors$sample_size)



pop_onset_plot = get_plot(population_errors, 'onset')
ggsave(filename = 'manuscript/population_onset_errors.png', plot = pop_onset_plot, dpi = 600, height = 20, width = 22, units = 'cm')

pop_end_plot = get_plot(population_errors, 'end')
ggsave(filename = 'manuscript/population_end_errors.png', plot = pop_end_plot, dpi = 600, height = 20, width = 22, units = 'cm')

pop_peak_plot = get_plot(population_errors, 'peak', x_lower_bound = -10, x_upper_bound = 10)
ggsave(filename = 'manuscript/population_peak_errors.png', plot = pop_peak_plot, dpi = 600, height = 20, width = 22, units = 'cm')

#############################################
# Error plots for individual estiamtes
individual_estimates = read_csv(individual_estimator_output_file) 

individual_true_data = read_csv(individual_true_flowering_dates_file) %>%
  gather(metric, actual_doy, -year, -plant_id)

individual_errors = individual_estimates %>%
  filter(!is.na(estimate), is.finite(estimate)) %>%
  left_join(individual_true_data, by=c('year','metric','plant_id')) %>%
  mutate(error = estimate - actual_doy)

individual_errors = individual_errors %>%
  group_by(metric, method, sample_size, percent_yes) %>%
  ungroup()

individual_errors$method = as.factor(individual_errors$method)
individual_errors$method = forcats::fct_recode(individual_errors$method, 'First Observed'='first_observed',
                                                                         'Midway' = 'midway',
                                                                         'Midway 7-Day' = 'midway_7day',
                                                                         'Logistic' = 'logistic',
                                                                         'Weibull Curve' = 'pearse')
individual_errors = individual_errors %>%
  mutate(sample_size_display = paste('Sample Size',sample_size, sep = ' : '),
         percent_yes_display = paste('Percent Yes',percent_yes, sep = ' : '))
individual_errors$sample_size_display = forcats::fct_reorder(individual_errors$sample_size_display, individual_errors$sample_size)



ind_onset_plot = get_plot(individual_errors, 'onset')
ggsave(filename = 'manuscript/individual_onset_errors.png', plot = ind_onset_plot, dpi = 600, height = 20, width = 22, units = 'cm')

ind_end_plot = get_plot(individual_errors, 'end')
ggsave(filename = 'manuscript/individual_end_errors.png', plot = ind_end_plot, dpi = 600, height = 20, width = 22, units = 'cm')

################################
library(kableExtra)

population_errors_text = population_errors %>%
  group_by(method, metric, sample_size, percent_yes) %>%
  summarise(median_error = round(median(error, na.rm = T),1),
            quantile_025_error = round(quantile(error, 0.025, na.rm = T),1),
            quantile_975_error= round(quantile(error, 0.975, na.rm = T),1)) %>%
  ungroup() %>%
  mutate(error_text = paste0(median_error,' (',quantile_025_error,', ',quantile_975_error,')')) %>%
  select(method, metric, sample_size, percent_yes, error_text) 

population_errors = population_errors %>%
  left_join(population_errors_text, by=c('metric','method','percent_yes','sample_size'))
