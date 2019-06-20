library(tidyverse)

source('config.R')
#########################################
# This script explores the GAM and Logistic methods further. The first part plots the performance (R^2 value) of 
# the methods using different probability thresholds.
#
# The 2nd section plots the GAM output across the full calendar year (DOY 1-365) for different scenarios, showing
# how the GAM curve can be overfit as the amount of absences change. 

#############################################

add_pretty_facet_text = function(df){
  df = df %>%
    mutate(sample_size_display = paste('Sample Size',sample_size, sep = ' : '),
           percent_yes_display = paste0('Presence Percent : ',percent_yes*100,'%'))
  df$sample_size_display = forcats::fct_reorder(df$sample_size_display, 
                                                df$sample_size)
  return(df)
}

##############################################
##############################################
##############################################
# Part 1, what is the best theshold in each scenario for the GAM and  Logistic methods?
##############################################
##############################################

# Get GAM and Logistic method results for all probability thresholds
gam_logistic_estimates = read_csv(population_estimator_output_file) %>%
  tidyr::extract(method, c('method', 'threshold'), regex="(\\D+)(\\d+|$)", convert=T) %>%
  mutate(threshold = threshold/100) %>%
  filter(method %in% c('gam','logistic'),
         metric %in% c('onset','end'))

population_true_data = read_csv(population_true_flowering_dates_file) %>%
  gather(metric, actual_doy, -year)

errors = gam_logistic_estimates %>%
  filter(!is.na(estimate), is.finite(estimate)) %>%
  left_join(population_true_data, by=c('year','metric')) %>%
  mutate(error = estimate - actual_doy) %>%
  group_by(method, threshold, metric, sample_size, percent_yes) %>%
  summarise(median_error = round(median(error, na.rm = T),0),
            quantile_025_error = round(quantile(error, 0.025, na.rm = T),0),
            quantile_975_error= round(quantile(error, 0.975, na.rm = T),0),
            R2 = 1 - (sum((estimate - actual_doy)**2) / sum((estimate - mean(actual_doy))**2))) %>%
  ungroup()

errors = add_pretty_facet_text(errors)

# For GAM
gam_threshold_plot = ggplot(filter(errors, method=='gam'), aes(x=threshold, y=R2, color=metric)) + 
  geom_line(size=1.5) +
  geom_point(size=3) + 
  scale_color_manual(values=c('black','grey60')) + 
  scale_x_continuous(breaks=unique(errors$threshold)) + 
  facet_wrap(sample_size_display~percent_yes_display)+
  theme_bw() + 
  theme(panel.grid.minor.x = element_blank(),
        strip.text = element_text(size=16),
        axis.text = element_text(size=12),
        axis.title = element_text(size=18),
        legend.key.width = unit(20,'mm'),
        legend.text = element_text(size=16),
        legend.title = element_text(size=18),
        legend.position = 'bottom') +
  labs(subtitle='GAM Performance Using Different Probability Thresholds',
       x='Probability Threshold', y='R^2', color='Metric')

ggsave(filename = 'manuscript/figs/fig_S4_gam_threshold_evaluation.png', plot = gam_threshold_plot, dpi = 200, height = 24, width = 28, units = 'cm')

# For Logistic
logistic_threshold_plot = ggplot(filter(errors, method=='logistic'), aes(x=threshold, y=R2, color=metric)) + 
  geom_line(size=1.5) +
  geom_point(size=3) + 
  scale_color_manual(values=c('black','grey60')) + 
  scale_x_continuous(breaks=unique(errors$threshold)) + 
  facet_wrap(sample_size_display~percent_yes_display)+
  theme_bw() + 
  theme(panel.grid.minor.x = element_blank(),
        strip.text = element_text(size=16),
        axis.text = element_text(size=12),
        axis.title = element_text(size=18),
        legend.key.width = unit(20,'mm'),
        legend.text = element_text(size=16),
        legend.title = element_text(size=18),
        legend.position = 'bottom') +
  labs(subtitle='Logistic Performance Using Different Probability Thresholds',
       x='Probability Threshold', y='R^2', color='Metric')

ggsave(filename = 'manuscript/figs/fig_S5_logistic_threshold_evaluation.png', plot = logistic_threshold_plot, dpi = 200, height = 24, width = 28, units = 'cm')

################################################################
################################################################
################################################################
################################################################
# Part 2, exploring how the  GAM/Logistic model curves change with different scenarios
source('estimators.R')

flowering_data = read_csv(population_data_for_estimators_file)
##################################################################
# 

all_probability_curves = data.frame()
all_observations = data.frame()
all_estimates = data.frame()

focal_year = 2007
focal_bootstrap = 8
gam_probability_threshold = 0.5
logistic_probability_threshold = 0.5

for(this_sample_size in population_sample_sizes){
  for(this_percent_yes in percent_yes){
    
    data_subset = flowering_data %>%
      filter(year == focal_year,
             sample_size == this_sample_size,
             percent_yes == this_percent_yes,
             bootstrap_i == focal_bootstrap)
    
    all_observations = all_observations %>%
      bind_rows(data_subset)
    #####################################
    #####################################
    # GAM estimates. 
    gam_model = tryCatch({mgcv::gam(flowering ~ s(doy), family=binomial, data=data_subset)},
                         error = function(x){return(NA)})
    
    if(is.na(gam_model)){
      next
    }
    all_doys = data.frame(doy = 1:365)
    all_doys$flowering_probability = predict(gam_model, newdata = all_doys, type = 'response')
    
    #####################
    peak_doy = all_doys$doy[which.max(all_doys$flowering_probability)]
    onset_doy = all_doys %>%
      filter(doy <= peak_doy, flowering_probability >= gam_probability_threshold) %>%
      pull(doy) %>%
      min()
    end_doy = all_doys %>%
      filter(doy >= peak_doy, flowering_probability <= gam_probability_threshold) %>%
      pull(doy) %>%
      min()
    this_subset_estimates = data.frame('metric' =c('onset','peak','end'),
                                       'estimate' = c(onset_doy, peak_doy, end_doy))
    this_subset_estimates$year = focal_year
    this_subset_estimates$sample_size = this_sample_size
    this_subset_estimates$percent_yes = this_percent_yes
    this_subset_estimates$bootstrap_i = focal_bootstrap
    this_subset_estimates$method = 'GAM'
    
    all_estimates = all_estimates %>%
      bind_rows(this_subset_estimates)
    #####################
    
    all_doys$year = focal_year
    all_doys$sample_size = this_sample_size
    all_doys$percent_yes = this_percent_yes
    all_doys$bootstrap_i = focal_bootstrap
    all_doys$method = 'GAM'
    
    all_probability_curves = all_probability_curves %>%
      bind_rows(all_doys)
    
    ######################################
    ######################################
    # Logistic estiamtes
    data_subset_for_logistic = drop_zeros(data_subset, type_to_drop = 'trailing')
    logistic_model = tryCatch({glm(flowering ~ doy, family=binomial, data=data_subset_for_logistic)},
                              error = function(x){return(NA)})
    all_doys = data.frame(doy = 1:365)
    all_doys$flowering_probability = predict(logistic_model, newdata = all_doys, type = 'response')
    
    onset_doy = all_doys %>%
      filter(flowering_probability >= logistic_probability_threshold) %>%
      pull(doy) %>%
      min()
    this_subset_estimates = data.frame('metric' =c('onset'),
                                       'estimate' = c(onset_doy))
    this_subset_estimates$method = 'Logistic'
    this_subset_estimates$year = focal_year
    this_subset_estimates$sample_size = this_sample_size
    this_subset_estimates$percent_yes = this_percent_yes
    this_subset_estimates$bootstrap_i = focal_bootstrap
    all_estimates = all_estimates %>%
      bind_rows(this_subset_estimates)
    
    ##########################
    #####################
    
    all_doys$year = focal_year
    all_doys$sample_size = this_sample_size
    all_doys$percent_yes = this_percent_yes
    all_doys$bootstrap_i = focal_bootstrap
    all_doys$method = 'Logistic'
    
    all_probability_curves = all_probability_curves %>%
      bind_rows(all_doys)
    
  }
}


all_estimates$metric = factor(all_estimates$metric, levels = c('onset','peak','end'), ordered=T)

all_estimates = add_pretty_facet_text(all_estimates)
all_probability_curves = add_pretty_facet_text(all_probability_curves)
all_observations = add_pretty_facet_text(all_observations)

gam_logistic_curve_plot = ggplot() + 
  geom_line(data = all_probability_curves, aes(x=doy, y=flowering_probability, linetype=method), size=1) +
  geom_jitter(data= all_observations, aes(x=doy, y=flowering),
              width = 0, height = 0.05) + 
  geom_vline(data=all_estimates, aes(xintercept = estimate, color=metric, linetype=method),size=1.2) +
  scale_color_manual(values=c('#56B4E9','#F0E442','#D55E00')) + 
  facet_wrap(sample_size_display~percent_yes_display) +
  labs(y='Flowering Probability',x='Day Of Year (DOY)', color='Estimate Type',
       linetype='Method') + 
  theme_bw() + 
  theme(legend.position = 'right',
        legend.key.width = unit(4,'mm'),
        legend.key.height = unit(15,'mm'),
        axis.title = element_text(size=20),
        axis.text = element_text(size=14),
        legend.title = element_text(size=20),
        legend.text = element_text(size=18),
        strip.text = element_text(size=16),
        strip.background = element_rect(fill='grey95'),
        panel.background = element_rect(fill='grey85'))

ggsave(filename = 'manuscript/figs/fig_S6_gam_logistic_curves.png', plot = gam_logistic_curve_plot, dpi = 100, height = 25, width = 35, units = 'cm')


