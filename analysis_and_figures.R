library(tidyverse)

source('config.R')


fl_estimates = read_csv(estimator_output_file) %>%
  filter(method != 'survival_curve_mean')

fl_true_data = read_csv(true_flowering_dates_file) %>%
  gather(flowering_type, actual_doy, -year)
  

avg_true_data = fl_true_data %>%
  group_by(flowering_type) %>%
  summarise(mean_doy = mean(actual_doy))
########################################################
#
error_estimates_per_year = fl_estimates %>%
  group_by(method, sample_size, percent_yes) %>%
  summarise(mean_doy_estimate = mean(estimate, na.rm=T),
            sd_doy_estimate = sd(estimate, na.rm=T),
            CI_95_low = quantile(estimate, 0.025, na.rm=T),
            CI_95_high = quantile(estimate, 0.975, na.rm=T)) %>%
  ungroup() 

# year_separater_lines = expand.grid(year = 2005:2015,
#                                    method = 'a')
# year_separater_lines$method = as.character(year_separater_lines$method)

method_labels = tribble(
  ~method, ~method_label,
  "naive_ffd", 'Naive FFD',
  "mean_flowering", "Mean Flowering",
  "midway_method", "Midway Method",
  "logistic", 'Logistic',
  "pearse", 'Weibul Curve',
  "survival_curve_median", 'Survival Curve'
)
method_labels$doy = 100

year_to_plot = 2008
error_estimates_per_year %>%
  ggplot(aes(x=mean_doy_estimate, y=method, color=method)) +
  #scale_y_continuous(labels = 2005:2015, breaks = 2005:2015) + 
  #xlim(98, 250) + 
  geom_vline(data = avg_true_data,
             aes(xintercept = mean_doy, linetype=flowering_type),
             size=1) + 
  scale_linetype_manual(values = c('dotted','longdash','solid')) + 
  geom_point(size=4, show.legend = FALSE) + 
  geom_errorbarh(aes(xmin =CI_95_low,
                     xmax = CI_95_high),
                 height = 0, size=1, show.legend = FALSE) + 
  geom_text(data = method_labels, aes(x=doy, label=method_label),
            hjust = 0, color='black', position = position_nudge(y=0.25)) + 
  scale_color_brewer(palette = 'Dark2') + 
  facet_wrap(sample_size~percent_yes, ncol=3, labeller = label_both) + 
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = 'none') +
  labs(y='',x='Julian Day (DOY)', title = paste('Estimates for flowering year ',year_to_plot))


ggplot(filter(error_estimates_per_year, flowering_type=='flowering_25_of_peak'), aes(x=actual_doy, y=mean_doy_estimate, color=method)) +
  geom_point() + 
  geom_smooth(method='lm', se=FALSE) + 
  scale_color_brewer(palette = 'Dark2') + 
  geom_abline(slope = 1, intercept = 0, size=1) + 
  facet_grid(sample_size~percent_yes)

#########################################################

error_estimates_summarized = error_estimates_per_year %>%
  group_by(method, sample_size, percent_yes, flowering_type) %>%
  summarise(mae = mean(abs(mean_doy_estimate - actual_doy)),
            neg_log_liklihood = -1 * sum(dnorm(actual_doy, mean = mean_doy_estimate, sd = sd_doy_estimate, log=T))) %>%
  ungroup()

summarized_errors_table = error_estimates_summarized %>%
  gather(error_metric, error_value, mae, neg_log_liklihood) %>%
  group_by(error_metric, sample_size, flowering_type, percent_yes) %>%
  mutate(error_value = round(error_value - min(error_value), 1)) %>% # Normalize to the lowest relative score
  ungroup() %>%
  mutate(method_metric = paste(error_metric, method,sep='-')) %>%
  select(-method, -error_metric) %>%
  spread(method_metric, error_value) %>%
  arrange(flowering_type,sample_size, percent_yes)

write_csv(summarized_errors_table, 'derived_data/error_table.csv')

ggplot(fl_estimates, aes(x = year, y=estimate, group=method, fill=method)) + 
  geom_boxplot() +
  #geom_hline(data = fl_true_data,aes(yintercept = doy, color=flowering_type)) +
  coord_flip() + 
  facet_wrap(year~sample_size~percent_yes, scales='free')


ggplot(filter(fl_estimates, year==2005), aes(x=estimate, fill=method)) + 
  geom_density() + 
  geom_vline(data = filter(fl_true_data, year==2005), aes(xintercept = doy, color=flowering_type)) +
  facet_wrap(sample_size~percent_yes)


########################################
# calculate error of every bootstrap estimate
errors = fl_estimates %>%
  left_join(fl_true_data, by='year') %>%
  group_by(method, flowering_type, sample_size, percent_yes) %>%
  summarize(medianAE = median(abs(estimate-actual_doy), na.rm=T),
            meanAE = mean(abs(estimate-actual_doy), na.rm=T),
            rmse = sqrt(mean((estimate-actual_doy)^2, na.rm=T))) %>%
  ungroup() %>%
  gather(error_metric, error_value, medianAE,meanAE, rmse) %>%
  group_by(error_metric, sample_size, flowering_type, percent_yes) %>%
  mutate(error_value = round(error_value - min(error_value), 1)) %>% # Normalize to the lowest relative score
  ungroup() %>%
  mutate(method_metric = paste(error_metric, method,sep='-')) %>%
  select(-method, -error_metric) %>%
  spread(method_metric, error_value) %>%
  arrange(flowering_type,sample_size, percent_yes)

#########################################
library(ggridges)
errors = fl_estimates %>%
  left_join(fl_true_data, by='year') %>%
  mutate(error = estimate - actual_doy)


ggplot(filter(errors, flowering_type=='first_flower'), aes(x=error, y=method)) + 
  geom_density_ridges(fill=NA, size=1, aes(color=method)) + 
  geom_vline(xintercept = 0, size=1) + 
  scale_color_brewer(palette = 'Dark2') + 
  xlim(-50,50) + 
  facet_wrap(sample_size~percent_yes) +
  theme_bw()


