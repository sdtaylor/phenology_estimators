library(tidyverse)

source('config.R')


fl_estimates = read_csv(estimator_output_file) %>%
  filter(method != 'survival_curve_mean')

fl_true_data = read_csv(true_flowering_dates_file) %>%
  gather(flowering_type, actual_doy, -year)

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
