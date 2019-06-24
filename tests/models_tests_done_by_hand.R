
######################################
# Here I essentially rewrite the estimator code to verify it's getting the results
# I expect. 
######################################

population_test_data = read_csv('tests/population_test_data.csv')

all_doys = data.frame(doy = 1:365)

###################
# logistic onset model
last_fl_doy = population_test_data %>%
  filter(flowering==1) %>%
  pull(doy) %>%
  max()

logistic_onset_model = glm(flowering~doy, family = binomial, data=filter(population_test_data, doy<=last_fl_doy))

all_doys$fl_probability = predict(logistic_onset_model, newdata = all_doys, type = 'response')

# 0.05 threshold, 169
all_doys %>%
  filter(fl_probability>=0.05) %>%
  pull(doy) %>%
  min()

# 0.25 threshold, 181
all_doys %>%
  filter(fl_probability>=0.25) %>%
  pull(doy) %>%
  min()

# 0.50 threshold, 188
all_doys %>%
  filter(fl_probability>=0.5) %>%
  pull(doy) %>%
  min()

# 0.75 threshold, 195
all_doys %>%
  filter(fl_probability>=0.75) %>%
  pull(doy) %>%
  min()

# 0.95 threshold, 207
all_doys %>%
  filter(fl_probability>=0.95) %>%
  pull(doy) %>%
  min()
###########################
# Logistic end  model

first_fl_doy = population_test_data %>%
  filter(flowering==1) %>%
  pull(doy) %>%
  min()

logistic_end_model = glm(flowering~doy, family = binomial, data=filter(population_test_data, doy>=first_fl_doy))

all_doys$fl_probability = predict(logistic_end_model, newdata = all_doys, type = 'response')

# 0.05 threshold, 276
all_doys %>%
  filter(fl_probability<=0.05) %>%
  pull(doy) %>%
  min()

# 0.25 threshold, 246
all_doys %>%
  filter(fl_probability<=0.25) %>%
  pull(doy) %>%
  min()

# 0.50 threshold, 228
all_doys %>%
  filter(fl_probability<=0.5) %>%
  pull(doy) %>%
  min()

# 0.75 threshold, 210
all_doys %>%
  filter(fl_probability<=0.75) %>%
  pull(doy) %>%
  min()

# 0.95 threshold, 180
all_doys %>%
  filter(fl_probability<=0.95) %>%
  pull(doy) %>%
  min()

##################################################
###################################################
###################
# gam onset model
library(mgcv)
gam_model = gam(flowering~s(doy), family = binomial, data=population_test_data)

all_doys$fl_probability = predict(gam_model, newdata = all_doys, type = 'response')

max_fl_prob_doy = all_doys$doy[which.max(all_doys$fl_probability)]

# 0.05 threshold, 173
all_doys %>%
  filter(doy<max_fl_prob_doy,fl_probability>=0.05) %>%
  pull(doy) %>%
  min()

# 0.25 threshold, 182
all_doys %>%
  filter(doy<max_fl_prob_doy,fl_probability>=0.25) %>%
  pull(doy) %>%
  min()

# 0.50 threshold, 188
all_doys %>%
  filter(doy<max_fl_prob_doy,fl_probability>=0.5) %>%
  pull(doy) %>%
  min()

# 0.75 threshold, 194
all_doys %>%
  filter(doy<max_fl_prob_doy,fl_probability>=0.75) %>%
  pull(doy) %>%
  min()

# 0.95 threshold, Inf (prob maxes out at  ~90)
all_doys %>%
  filter(doy<max_fl_prob_doy,fl_probability>=0.95) %>%
  pull(doy) %>%
  min()

##################################################
###################################################
###################
# gam end. uses the same model as onset

# 0.05 threshold, 227
all_doys %>%
  filter(doy>max_fl_prob_doy,fl_probability<=0.05) %>%
  pull(doy) %>%
  min()

# 0.25 threshold, 223
all_doys %>%
  filter(doy > max_fl_prob_doy, fl_probability <= 0.25) %>%
  pull(doy) %>%
  min()

# 0.50 threshold, 220
all_doys %>%
  filter(doy > max_fl_prob_doy, fl_probability <= 0.5) %>%
  pull(doy) %>%
  min()

# 0.75 threshold, 216
all_doys %>%
  filter(doy > max_fl_prob_doy, fl_probability <= 0.75) %>%
  pull(doy) %>%
  min()

# 0.95 threshold, 207
all_doys %>%
  filter(doy > max_fl_prob_doy, fl_probability <= 0.95) %>%
  pull(doy) %>%
  min()


##################################################################################
##################################################################################
# first and last observed methods

# first observed, 191
population_test_data %>%
  filter(flowering==1) %>%
  pull(doy) %>%
  min()

# last observed, 214
population_test_data %>%
  filter(flowering==1) %>%
  pull(doy) %>%
  max()
