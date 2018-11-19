library(tidyverse)

source('config.R')


fl_estimates = read_csv(estimator_output_file) 

fl_true_data = read_csv(true_flowering_dates_file) %>%
  gather(flowering_type, actual_doy, -year)

#########################################
library(ggridges)
errors = fl_estimates %>%
  left_join(fl_true_data, by='year') %>%
  mutate(error = estimate - actual_doy)

errors$method = as.factor(errors$method)
errors$method = forcats::fct_recode(errors$method, 'Naive FFD'='naive_ffd',
                                                   'Mean Flowering' = 'mean_flowering',
                                                   'Survival Curve' = 'survival_curve_median',
                                                   'Midway' = 'midway_method',
                                                   'Logistic' = 'logistic',
                                                   'Weibull Curve' = 'pearse')
errors = errors %>%
  mutate(sample_size_display = paste('Sample Size',sample_size, sep = ' : '),
         percent_yes_display = paste('Percent Yes',percent_yes, sep = ' : '))
errors$sample_size_display = forcats::fct_reorder(errors$sample_size_display, errors$sample_size)

get_plot = function(flowering_type_to_plot){
  p=ggplot(filter(errors, flowering_type==flowering_type_to_plot), aes(x=error, y=method)) + 
    geom_density_ridges(fill=NA, size=1.5, aes(color=method)) + 
    geom_vline(xintercept = 0, size=1) + 
    scale_color_brewer(palette = 'Dark2') + 
    #scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9", "#0072B2", "#D55E00", "#CC79A7")) + 
    xlim(-50,50) + 
    facet_wrap(sample_size_display~percent_yes_display) +
    theme_light() + 
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

first_fl_plot = get_plot('first_flower')
ggsave(filename = 'manuscript/first_flower_errors.png', plot = first_fl_plot, dpi = 600, height = 20, width = 22, units = 'cm')

flowering_50_of_peak_plot = get_plot('flowering_25_of_peak')
ggsave(filename = 'manuscript/flowering_50_of_peak_errors.png', plot = flowering_50_of_peak_plot, dpi = 600, height = 20, width = 22, units = 'cm')

peak_flower_plot = get_plot('peak_flower')
ggsave(filename = 'manuscript/peak_flower_plot.png', plot = peak_flower_plot, dpi = 600, height = 20, width = 22, units = 'cm')





