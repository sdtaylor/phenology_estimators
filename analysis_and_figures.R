library(tidyverse)

source('config.R')

#########################################
library(ggridges)

get_plot = function(error_df, metric_to_plot,
                    x_lower_bound=-50, x_upper_bound=50,
                    error_text_x_placement, error_text_y_nudge=0.5,
                    r2_text_x_placement, r2_text_y_nudge=0.5){
  
  error_text_only = error_df %>%
    filter(metric==metric_to_plot) %>%
    select(method, sample_size_display, percent_yes_display, error_text, r2_text) %>%
    distinct()
  
  p=  ggplot(filter(error_df, metric==metric_to_plot), aes(x=error, y=method)) + 
    geom_density_ridges(fill=NA, size=1.5, aes(color=method), panel_scaling=FALSE) + 
    geom_label(data=error_text_only, aes(x=error_text_x_placement, label=error_text), inherit.aes = TRUE,
              hjust=0, nudge_y =error_text_y_nudge, size=2.5, label.size = 0, alpha=0.8) +
    geom_label(data=error_text_only, aes(x=r2_text_x_placement, label=r2_text), inherit.aes = TRUE,
               hjust=0, nudge_y =r2_text_y_nudge, size=2.3, label.size = 0, alpha=0.8, parse=TRUE) +
    geom_vline(xintercept = 0, size=1) + 
    scale_color_brewer(palette = 'Dark2') + 
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

# Drop Mean Midway 7-Day with sample size of 10 cause over 99% of the estimates
# were dropped and the final error is inflated.
population_estimates = population_estimates %>%
  filter(!(method=='mean_midway_7day' & sample_size==10))

population_true_data = read_csv(population_true_flowering_dates_file) %>%
  gather(metric, actual_doy, -year)

population_errors = population_estimates %>%
  filter(!is.na(estimate), is.finite(estimate)) %>%
  left_join(population_true_data, by=c('year','metric')) %>%
  mutate(error = estimate - actual_doy)

# Calculate the median and CI's of density curves in the plots
population_errors_text = population_errors %>%
  group_by(method, metric, sample_size, percent_yes) %>%
  summarise(median_error = round(median(error, na.rm = T),0),
            quantile_025_error = round(quantile(error, 0.025, na.rm = T),0),
            quantile_975_error= round(quantile(error, 0.975, na.rm = T),0),
            R2 = 1 - (sum((estimate - actual_doy)**2) / sum((estimate - mean(actual_doy))**2))) %>%
  ungroup() %>%
  mutate(error_text = paste0(median_error,' (',quantile_975_error - quantile_025_error,')'),
         r2_text = paste0('R^2 == ',round(R2,2))) %>%
  select(method, metric, sample_size, percent_yes, error_text, r2_text) 

population_errors = population_errors %>%
  left_join(population_errors_text, by=c('metric','method','percent_yes','sample_size'))

# Nicer looking names for everything
population_errors = population_errors %>%
  mutate(method = ifelse((method=='first_observed' & metric=='end'), 'last_observed',method))
population_errors$method = as.factor(population_errors$method)
population_errors$method = forcats::fct_recode(population_errors$method, 'First Observed'='first_observed',
                                                   'Last Observed'='last_observed',
                                                   'Mean Flowering' = 'mean_flowering',
                                                   'Survival Curve' = 'survival_curve_median',
                                                   'Mean Midway' = 'mean_midway',
                                                   'Mean Midway 7-Day' = "mean_midway_7day",
                                                   'Logistic' = 'logistic',
                                                   'GAM' = 'gam',
                                                   'Weibull' = 'pearse')
population_errors$method = fct_relevel(population_errors$method,'First Observed','Last Observed','GAM','Logistic','Mean Midway','Mean Midway 7-Day','Mean Flowering','Survival Curve','Weibull')

population_errors = population_errors %>%
  mutate(sample_size_display = paste0('Sample Size : ',sample_size),
         percent_yes_display = paste0('Presence Percent : ',percent_yes*100,'%'))
population_errors$sample_size_display = forcats::fct_reorder(population_errors$sample_size_display, population_errors$sample_size)

############################
percent_of_estimates_kept = population_errors %>%
  group_by(method ,metric, sample_size, percent_yes) %>%
  summarise(total_estimates = n()) %>%
  ungroup() %>%
  mutate(percent_kept = total_estimates/11000)

############################
# figures
pop_onset_plot = get_plot(population_errors, 'onset', error_text_x_placement = -50, r2_text_x_placement = 30)
ggsave(filename = 'manuscript/figs/fig_1_population_onset_errors.png', plot = pop_onset_plot, dpi = 600, height = 20, width = 22, units = 'cm')

pop_end_plot = get_plot(population_errors, 'end', error_text_x_placement = -50, error_text_y_nudge = 0.45, r2_text_x_placement = 30)
ggsave(filename = 'manuscript/figs/fig_2_population_end_errors.png', plot = pop_end_plot, dpi = 600, height = 20, width = 22, units = 'cm')

pop_peak_plot = get_plot(population_errors, 'peak', x_lower_bound = -10, x_upper_bound = 10, error_text_x_placement = -9, error_text_y_nudge = 0.6,
                         r2_text_x_placement = 5)
ggsave(filename = 'manuscript/figs/fig_3_population_peak_errors.png', plot = pop_peak_plot, dpi = 600, height = 20, width = 22, units = 'cm')

#############################################
# Error plots for individual estiamtes
individual_estimates = read_csv(individual_estimator_output_file) %>%
  filter(metric!='peak')
#TODO: peak estimates snuck in there from the gam estimate, remove it from the run_estimates file later

individual_true_data = read_csv(individual_true_flowering_dates_file) %>%
  gather(metric, actual_doy, -year, -plant_id)

individual_errors = individual_estimates %>%
  filter(!is.na(estimate), is.finite(estimate)) %>%
  left_join(individual_true_data, by=c('year','metric','plant_id')) %>%
  mutate(error = estimate - actual_doy)

# Calculate the median and CI's of density curves in the plots
individual_errors_text = individual_errors %>%
  group_by(method, metric, sample_size, percent_yes) %>%
  summarise(median_error = round(median(error, na.rm = T),0),
            quantile_025_error = round(quantile(error, 0.025, na.rm = T),0),
            quantile_975_error= round(quantile(error, 0.975, na.rm = T),0),
            R2 = 1 - (sum((estimate - actual_doy)**2) / sum((estimate - mean(actual_doy))**2))) %>%
  ungroup() %>%
  mutate(error_text = paste0(median_error,' (',quantile_975_error - quantile_025_error,')'),
         r2_text = paste0('R^2 == ',round(R2,2))) %>%
  select(method, metric, sample_size, percent_yes, error_text, r2_text) 

individual_errors = individual_errors %>%
  left_join(individual_errors_text, by=c('metric','method','percent_yes','sample_size'))

# nice names for the  figures
individual_errors = individual_errors %>%
  mutate(method = ifelse((method=='first_observed' & metric=='end'), 'last_observed',method))

individual_errors$method = as.factor(individual_errors$method)
individual_errors$method = forcats::fct_recode(individual_errors$method, 'First Observed'='first_observed',
                                                                         'Last Observed'='last_observed',
                                                                         'Midway' = 'midway',
                                                                         'Midway 7-Day' = 'midway_7day',
                                                                         'Logistic' = 'logistic',
                                                                         'GAM' = 'gam',
                                                                         'Weibull' = 'pearse')
individual_errors$method = fct_relevel(individual_errors$method,'First Observed','Last Observed','GAM','Logistic','Midway','Midway 7-Day','Weibull')

individual_errors = individual_errors %>%
  mutate(sample_size_display = paste('Sample Size',sample_size, sep = ' : '),
         percent_yes_display = paste0('Presence Percent : ',percent_yes*100,'%'))
individual_errors$sample_size_display = forcats::fct_reorder(individual_errors$sample_size_display, individual_errors$sample_size)


ind_onset_plot = get_plot(individual_errors, 'onset', error_text_x_placement = -50, r2_text_x_placement = 30)
ggsave(filename = 'manuscript/figs/fig_4_individual_onset_errors.png', plot = ind_onset_plot, dpi = 600, height = 20, width = 22, units = 'cm')

ind_end_plot = get_plot(individual_errors, 'end', error_text_x_placement = -50, r2_text_x_placement = 30)
ggsave(filename = 'manuscript/figs/fig_S3_individual_end_errors.png', plot = ind_end_plot, dpi = 600, height = 20, width = 22, units = 'cm')


