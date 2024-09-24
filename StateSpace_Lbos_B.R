##############################################################################
### Building a state-space model to explore streamflow at Jonkershoek
###   https://doi.org/10.1002/hyp.14101.
### Based on this tutorial: https://course.naturecast.org/lessons/r-state-space-models-1/r_tutorial_mvgam/
##############################################################################

library(mvgam)
library(tidyverse)
library(slider)
library(ggpubr)

# Get data
flow <- read_csv("C:/Users/Timothy Muthama/OneDrive - University of Cape Town/Documents/Third Year/Second semester/BIO3019S/Independent project/R/Jonkershoek_SSM_Tim/data/Lambrechtbos_B_Belfort_stream_flow_volume_March_1961_October_2008.csv")
rain <- read_csv("data/10B_manual_monthly_rainfall_Apr1944_Mar1991.csv")


# Aggregate to monthly flows
mflow <- flow %>% 
  mutate(YearMon = format(Date, '%Y-%b')) %>%
  group_by(YearMon) %>%
  summarise(flow = sum(Value, na.rm = T))


# Format date column, add season column for rainfall and join streamflow
dat <- rain %>% mutate(YearMon = format(Date, '%Y-%b')) %>%
  mutate(season = match(format(Date, '%b'), month.abb)) %>%
  rename(rainfall = Value) %>%
  select(YearMon, rainfall, season, Date) %>%
  left_join(mflow) %>%
  na.omit() %>%
  filter(flow > 0)

# Add "time" as a numbered vector 1,2,3,etc
dat <- dat %>%
  mutate(time = rev(1:nrow(dat))) %>%
  arrange(time)

##Time series stuff##

rain <- read_csv("C:/Users/Timothy Muthama/OneDrive - University of Cape Town/Documents/Third Year/Second semester/BIO3019S/Independent project/R/Jonkershoek_SSM_Tim/data/10B_manual_monthly_rainfall_Apr1944_Mar1991.csv")

# SERIES 1

df1 <- read_csv("C:/Users/Timothy Muthama/OneDrive - University of Cape Town/Documents/Third Year/Second semester/BIO3019S/Independent project/R/Jonkershoek_SSM_Tim/data/Lambrechtbos_B_Belfort_stream_flow_volume_March_1961_October_2008.csv")

# Aggregate to monthly flows - SERIES 1
m.df1 <- df1 %>% 
  mutate(YearMon = format(Date, '%Y-%b')) %>%
  group_by(YearMon) %>%
  summarise(flow = sum(Value, na.rm = T))


# Format date column, add season column for rainfall and join streamflow - SERIES 1
dat.1 <- rain %>% mutate(YearMon = format(Date, '%Y-%b')) %>%
  mutate(season = match(format(Date, '%b'), month.abb)) %>%
  rename(rainfall = Value) %>%
  select(YearMon, rainfall, season, Date) %>%
  left_join(m.df1) %>%
  na.omit() %>%
  filter(flow > 0)

# Add "time" as a numbered vector 1,2,3,etc - SERIES 1
dat.1 <- dat.1 %>%
  mutate(time = rev(1:nrow(dat.1))) %>%
  arrange(time)

dat.1$series_id <- "1"
dat.1$series_id <- as.integer(dat.1$series_id)

dat.1 %>% mutate(lag_1 = lag(rainfall, 1),
               lag_2 = lag(rainfall, 2),
               lag_3 = lag(rainfall, 3)) %>%
  pivot_longer(cols = starts_with("lag")) %>%
  ggscatter(y = "flow", x = "value", add = "reg.line") +
  stat_cor(label.y = 85000) +
  stat_regline_equation(label.y = 80000) +
  # ggplot(aes(y = flow, x = value)) +
  # geom_point() +
  facet_wrap(.~name, scales = "free") 

#Adding lag and cum effect to series 1

dat.1 %>% mutate(lag_1. = lag(flow, 1.),
               lag_2. = lag(flow, 2.),
               lag_3. = lag(flow, 3.)) %>%
  pivot_longer(cols = starts_with("lag")) %>%
  ggscatter(y = "rainfall", x = "value", add = "reg.line") +
  stat_cor(label.y = 85000) +
  stat_regline_equation(label.y = 80000) +
  # ggplot(aes(y = flow, x = value)) +
  # geom_point() +
  facet_wrap(.~name, scales = "free")

dat.1 %>% mutate(cum_rain2 = slide_vec(rainfall, mean, .before = 1),
               cum_rain3 = slide_vec(rainfall, mean, .before = 2),
               cum_rain4 = slide_vec(rainfall, mean, .before = 3),
               cum_rain5 = slide_vec(rainfall, mean, .before = 4),
               cum_rain6 = slide_vec(rainfall, mean, .before = 5)) %>%
  pivot_longer(cols = starts_with("cum")) %>%
  ggscatter(y = "flow", x = "value", add = "reg.line") +
  stat_cor(label.y = 85000) +
  stat_regline_equation(label.y = 78000) +
  # ggplot(aes(y = flow, x = value)) +
  # geom_point() +
  # geom_smooth(method = "lm") +
  facet_wrap(.~name, scales = "free")

# Add lagged or cumulative rainfall to data
dat.1 <- dat.1 %>% 
  mutate(lag_1 = lag(rainfall, 1),
         cum_rain5 = slide_vec(rainfall, mean, .before = 4))


# SERIES 2

df2 <- read_csv("C:/Users/Timothy Muthama/OneDrive - University of Cape Town/Documents/Third Year/Second semester/BIO3019S/Independent project/R/Jonkershoek_SSM_Tim/data/Lambrechtbos_A_Belfort_stream_flow_volume_March_1961_October_2008.csv")

# Aggregate to monthly flows - SERIES 2
m.df2 <- df2 %>% 
  mutate(YearMon = format(Date, '%Y-%b')) %>%
  group_by(YearMon) %>%
  summarise(flow = sum(Value, na.rm = T))


# Format date column, add season column for rainfall and join streamflow - SERIES 2
dat.2 <- rain %>% mutate(YearMon = format(Date, '%Y-%b')) %>%
  mutate(season = match(format(Date, '%b'), month.abb)) %>%
  rename(rainfall = Value) %>%
  select(YearMon, rainfall, season, Date) %>%
  left_join(m.df2) %>%
  na.omit() %>%
  filter(flow > 0)

# Add "time" as a numbered vector 1,2,3,etc - SERIES 2
dat.2 <- dat.2 %>%
  mutate(time = rev(1:nrow(dat.2))) %>%
  arrange(time)

dat.2$series_id <- "2"
dat.2$series_id <- as.integer(dat.2$series_id)

#Adding lag and cum effect to series 2

dat.2 %>% mutate(lag_1. = lag(flow, 1.),
                 lag_2. = lag(flow, 2.),
                 lag_3. = lag(flow, 3.)) %>%
  pivot_longer(cols = starts_with("lag")) %>%
  ggscatter(y = "rainfall", x = "value", add = "reg.line") +
  stat_cor(label.y = 85000) +
  stat_regline_equation(label.y = 80000) +
  # ggplot(aes(y = flow, x = value)) +
  # geom_point() +
  facet_wrap(.~name, scales = "free")

dat.2 %>% mutate(cum_rain2 = slide_vec(rainfall, mean, .before = 1),
                 cum_rain3 = slide_vec(rainfall, mean, .before = 2),
                 cum_rain4 = slide_vec(rainfall, mean, .before = 3),
                 cum_rain5 = slide_vec(rainfall, mean, .before = 4),
                 cum_rain6 = slide_vec(rainfall, mean, .before = 5)) %>%
  pivot_longer(cols = starts_with("cum")) %>%
  ggscatter(y = "flow", x = "value", add = "reg.line") +
  stat_cor(label.y = 85000) +
  stat_regline_equation(label.y = 78000) +
  # ggplot(aes(y = flow, x = value)) +
  # geom_point() +
  # geom_smooth(method = "lm") +
  facet_wrap(.~name, scales = "free")

# Add lagged or cumulative rainfall to data
dat.2 <- dat.2 %>% 
  mutate(lag_1 = lag(rainfall, 1),
         cum_rain5 = slide_vec(rainfall, mean, .before = 4))


# SERIES 3

df3 <- read_csv("C:/Users/Timothy Muthama/OneDrive - University of Cape Town/Documents/Third Year/Second semester/BIO3019S/Independent project/R/Jonkershoek_SSM_Tim/data/Langrivier_Belfort_stream_flow_volume_March_1961_October_2008.csv")

# Aggregate to monthly flows - SERIES 3
m.df3 <- df3 %>% 
  mutate(YearMon = format(Date, '%Y-%b')) %>%
  group_by(YearMon) %>%
  summarise(flow = sum(Value, na.rm = T))


# Format date column, add season column for rainfall and join streamflow - SERIES 3
dat.3 <- rain %>% mutate(YearMon = format(Date, '%Y-%b')) %>%
  mutate(season = match(format(Date, '%b'), month.abb)) %>%
  rename(rainfall = Value) %>%
  select(YearMon, rainfall, season, Date) %>%
  left_join(m.df3) %>%
  na.omit() %>%
  filter(flow > 0)

# Add "time" as a numbered vector 1,2,3,etc - SERIES 3
dat.3 <- dat.3 %>%
  mutate(time = rev(1:nrow(dat.3))) %>%
  arrange(time)

dat.3$series_id <- "3"
dat.3$series_id <- as.integer(dat.3$series_id)

#Adding lag and cum effect to series 3

dat.3 %>% mutate(lag_1. = lag(flow, 1.),
                 lag_2. = lag(flow, 2.),
                 lag_3. = lag(flow, 3.)) %>%
  pivot_longer(cols = starts_with("lag")) %>%
  ggscatter(y = "rainfall", x = "value", add = "reg.line") +
  stat_cor(label.y = 85000) +
  stat_regline_equation(label.y = 80000) +
  # ggplot(aes(y = flow, x = value)) +
  # geom_point() +
  facet_wrap(.~name, scales = "free")

dat.3 %>% mutate(cum_rain2 = slide_vec(rainfall, mean, .before = 1),
                 cum_rain3 = slide_vec(rainfall, mean, .before = 2),
                 cum_rain4 = slide_vec(rainfall, mean, .before = 3),
                 cum_rain5 = slide_vec(rainfall, mean, .before = 4),
                 cum_rain6 = slide_vec(rainfall, mean, .before = 5)) %>%
  pivot_longer(cols = starts_with("cum")) %>%
  ggscatter(y = "flow", x = "value", add = "reg.line") +
  stat_cor(label.y = 85000) +
  stat_regline_equation(label.y = 78000) +
  # ggplot(aes(y = flow, x = value)) +
  # geom_point() +
  # geom_smooth(method = "lm") +
  facet_wrap(.~name, scales = "free")

# Add lagged or cumulative rainfall to data
dat.3 <- dat.3 %>% 
  mutate(lag_1 = lag(rainfall, 1),
         cum_rain5 = slide_vec(rainfall, mean, .before = 4))



# Creating the data sets for the time series models

dat.s <- bind_rows(dat.1, dat.2, dat.3)
dat.s.1 <- bind_rows(dat.1, dat.3)

d.train.s <- bind_rows(
  slice(dat.s, 1:129),    
  slice(dat.s, 359:392), 
  slice(dat.s, 718:847))  #Before pines
d.test.s <- bind_rows(
  slice(dat.s, 130:142),    
  slice(dat.s, 393:405), 
  slice(dat.s, 848:860))  # After pines

sum(is.na(d.train.s))
missing_rows <- which(is.na(d.train.s))
missing_data <- d.train.s[missing_rows, ]
print(missing_data)
d.train.s <- na.omit(d.train.s)

sum(is.na(d.test.s))
missing_rows <- which(is.na(d.test.s))
missing_data <- d.test.s[missing_rows, ]
print(missing_data)
d.test.s <- na.omit(d.test.s)

#attempt with a smaller data set
d.train.s.1 <- bind_rows(
  slice(dat.s, 1:129), 
  slice(dat.s, 718:846))  #Before pines
d.test.s.1 <- bind_rows(
  slice(dat.s, 130:142), 
  slice(dat.s, 847:859))  # After pines

sum(is.na(d.train.s.1))
missing_rows <- which(is.na(d.train.s.1))
missing_data <- d.train.s.1[missing_rows, ]
print(missing_data)
d.train.s.1 <- na.omit(d.train.s.1)

sum(is.na(d.test.s.1))
missing_rows <- which(is.na(d.test.s.1))
missing_data <- d.test.s.1[missing_rows, ]
print(missing_data)
d.test.s.1 <- na.omit(d.test.s.1)


trend_map <- data.frame(
  series = unique(dat.s.1$series_id),  # Extract the unique series from your data
  trend = c(1, 2))

######


ggplot(dat, aes(x = time, y = rainfall)) + # Rainfall over time
      geom_point() +  # Plot points
      geom_smooth(method = "lm", col = "blue", se = FALSE) 
      labs(x = "time ", y = "rainfall", title = "Scatter Plot of Rainfall       over time") +
      theme_minimal()

    
cor_rainfall <- round(cor(dat$rainfall, dat$time), 2)
lm_rainfall <- lm(rainfall ~ time, data = dat)
summary(lm_rainfall)

ggplot(dat, aes(x = time, y = flow)) + # Streamflow over time
       geom_point() +  # Plot points
       geom_smooth(method = "lm", col = "blue", se = FALSE) 
       labs(x = "time ", y = "flow", title = "Scatter Plot o                     Streamflow over time") +
       theme_minimal()

cor_streamflow <- round(cor(dat$flow, dat$time), 2) 
lm_flow <- lm(flow ~ time, data = dat)
summary(lm_flow)



# Explore lagged or cumulative effects of rainfall (also see RcppRoll::roll_mean or roll_sum that allow weighted cumulative windows)
# here we are looking at the dependency on of a data point on data points before it
# we can see that rainfall in the month before has a large impact on the the data at a point

dat %>% mutate(lag_1 = lag(rainfall, 1),
               lag_2 = lag(rainfall, 2),
               lag_3 = lag(rainfall, 3)) %>%
  pivot_longer(cols = starts_with("lag")) %>%
  ggscatter(y = "flow", x = "value", add = "reg.line") +
  stat_cor(label.y = 85000) +
  stat_regline_equation(label.y = 80000) +
  # ggplot(aes(y = flow, x = value)) +
  # geom_point() +
  facet_wrap(.~name, scales = "free") 

#don't pick up an auto-correlation with stream flow. Is this an error in my code or is this an actual trend
dat %>% mutate(lag_1. = lag(flow, 1.),
               lag_2. = lag(flow, 2.),
               lag_3. = lag(flow, 3.)) %>%
  pivot_longer(cols = starts_with("lag")) %>%
  ggscatter(y = "rainfall", x = "value", add = "reg.line") +
  stat_cor(label.y = 85000) +
  stat_regline_equation(label.y = 80000) +
  # ggplot(aes(y = flow, x = value)) +
  # geom_point() +
  facet_wrap(.~name, scales = "free")

dat %>% mutate(cum_rain2 = slide_vec(rainfall, mean, .before = 1),
               cum_rain3 = slide_vec(rainfall, mean, .before = 2),
               cum_rain4 = slide_vec(rainfall, mean, .before = 3),
               cum_rain5 = slide_vec(rainfall, mean, .before = 4),
               cum_rain6 = slide_vec(rainfall, mean, .before = 5)) %>%
  pivot_longer(cols = starts_with("cum")) %>%
  ggscatter(y = "flow", x = "value", add = "reg.line") +
  stat_cor(label.y = 85000) +
  stat_regline_equation(label.y = 78000) +
  # ggplot(aes(y = flow, x = value)) +
  # geom_point() +
  # geom_smooth(method = "lm") +
  facet_wrap(.~name, scales = "free")

# Add lagged or cumulative rainfall to data
dat <- dat %>% 
  mutate(lag_1 = lag(rainfall, 1),
         cum_rain5 = slide_vec(rainfall, mean, .before = 4))

# Split datasets
data_train <- slice(dat, 1:129) # before pines
data_pines <- slice(dat, 130:nrow(dat)) # with pines
data_test <- slice(dat, 130:142) # with pines and NAs
data_new <- slice(dat, 130:142) %>% mutate(flow = NA)


# Model
?mvgam
###### gaussian ######
baseline_model_g = mvgam(flow ~ rainfall ,
                       trend_model = "AR1",
                       family = gaussian(),
                       data = data_train,
                       newdata = data_test,
                       noncentred = T,
                       burnin = 5000,
                       samples = 5000,
                       thin = 9)

pairs(baseline_model)
mcmc_plot(baseline_model, type = "trace", variable = c("rainfall", "ar1[1]", "sigma[1]"))
plot(baseline_model, type = "forecast")
summary(baseline_model)

#guassian with seasonality

season_model_g = mvgam(flow ~ rainfall + s(season, bs = "cc") ,
                       trend_model = "AR1",
                       family = gaussian(),
                       data = data_train,
                       newdata = data_test,
                       noncentred = T,
                       burnin = 5000,
                       samples = 5000,
                       thin = 9)

pairs(season_model_g)
mcmc_plot(season_model_g, type = "trace", variable = c("rainfall", "ar1[1]", "sigma[1]"))
plot(season_model_g, type = "forecast")
summary(season_model_g)

#guassian with seasonality

season_model_g = mvgam(flow ~ rainfall + s(season, bs = "cc") ,
                       trend_model = "AR1",
                       family = gaussian(),
                       data = data_train,
                       newdata = data_test,
                       noncentred = T,
                       burnin = 5000,
                       samples = 5000,
                       thin = 9)

pairs(season_model_g)
mcmc_plot(season_model_g, type = "trace", variable = c("rainfall")
          , "ar1[1]", "sigma[1]"))
plot(season_model_g, type = "forecast")
summary(season_model_g)

# gaussian with a smooth term

rainfall_model_g = mvgam(flow ~ s(rainfall, bs = "cc"),
                       trend_model = "AR1",
                       family = gaussian(),
                       data = data_train,
                       newdata = data_test,
                       noncentred = T,
                       burnin = 5000,
                       samples = 5000,
                       thin = 9)

pairs(rainfall_model_g)
mcmc_plot(rainfall_model_g, type = "trace", variable = c("s(rainfall)_rho", "ar1[1]", "sigma[1]"))
plot(rainfall_model_g, type = "forecast")
summary(rainfall_model_g)

# Guassian with smooth terms for the seasonality and rainfall

mixed_model_g = mvgam(flow ~ s(rainfall, bs = "cc") + s(season, bs = "cc"),
                       trend_model = "AR1",
                       family = gaussian(),
                       data = data_train,
                       newdata = data_test,
                       noncentred = T,
                       burnin = 5000,
                       samples = 5000,
                       thin = 9)

pairs(mixed_model_g)
mcmc_plot(mixed_model_g, type = "trace", variable = c("s(rainfall)_rho", "s(season)_rho", "ar1[1]", "sigma[1]"))
plot(mixed_model_g, type = "forecast")
summary(mixed_model_g)

# gaussian with different trend_models
tread_model = mvgam(flow ~ rainfall ,
                       trend_model = "AR3",
                       family = gaussian(),
                       data = data_train,
                       newdata = data_test,
                       noncentred = T,
                       burnin = 5000,
                       samples = 5000,
                       thin = 9)
?pairs
pairs(baseline_model)
mcmc_plot(baseline_model, type = "trace", variable = c("rainfall", "ar3[1]", "sigma[1]"))
plot(baseline_model, type = "forecast")
summary(baseline_model)

# Time series 
full_mod <- mvgam(flow ~ rainfall + s(season, bs = "cc"),
                  trend_model = "AR1",
                  family = lognormal(),
                  data = d.train.s,
                  newdata = d.test.s,
                  noncentred = T,
                  burnin = 5000,
                  samples = 5000,
                  adapt_delta = 0.9,
                  thin = 9)

# attempt with smaller data set
baseline_model_s = mvgam(flow ~ rainfall ,
                         trend_model = "AR1",
                         family = gaussian(),
                         data = d.train.s.1,
                         newdata = d.test.s.1,
                         noncentred = T,
                         burnin = 5000,
                         samples = 5000,
                         thin = 9)

library(dplyr)

### Attempt using website

full_mod <- mvgam(flow ~ series_id - 1,
                  trend_formula = ~ rainfall,
                  trend_model = "AR1",
                  noncentred = TRUE,
                  trend_map = trend_map,
                  family = poisson(),
                  data = d.train.s.1,
                  silent = 2)

# Group by 'series_id' to check each series individually
missing_data <- d.train.s %>%
  group_by(series_id) %>%
  summarise(missing_timepoints = sum(is.na(flow))) 

print(missing_data)

###### lognormal ###### 1
lognormal_model = mvgam(flow ~ rainfall + s(season, bs = "cc"),
                       trend_model = "AR1",
                       family = lognormal(),
                       data = data_train,
                       newdata = data_test,
                       noncentred = T,
                       burnin = 5000,
                       samples = 5000,
                       adapt_delta = 0.9,
                       thin = 9)

pairs(lognormal_model)
mcmc_plot(lognormal_model, type = "trace", variable = c("rainfall", "ar1[1]", "sigma[1]"))
plot(lognormal_model, type = "forecast")
plot(lognormal_model, type = 'residuals')
pp_check(lognormal_model, type = 'dens_overlay')
pp_check(lognormal_model, type = 'pit_ecdf')
summary(lognormal_model)
mcmc_plot(lognormal_model, variable = c('rainfall'), regex = TRUE, type = 'areas')

# lognormal with seasonality
season_lognormal = mvgam(flow ~ rainfall+ s(season),
                        trend_model = "AR1",
                        family = lognormal(),
                        data = data_train,
                        newdata = data_test,
                        noncentred = T,
                        burnin = 5000,
                        samples = 5000,
                        adapt_delta = 0.9,
                        thin = 9)

pairs(season_lognormal)
mcmc_plot(season_lognormal, type = "trace", variable = c("rainfall", "ar1[1]", "sigma[1]"))
plot(season_lognormal, type = "forecast")
plot(season_lognormal, type = 'residuals')
pp_check(season_lognormal, type = 'dens_overlay')
pp_check(season_lognormal, type = 'pit_ecdf')
summary(season_lognormal)
mcmc_plot(season_lognormal, variable = c('rainfall'), regex = TRUE, type = 'areas')

# lognormal with a smooth term
rainfall_lognormal = mvgam(flow ~ s(rainfall, bs = "cc"),
                         trend_model = "AR1",
                         family = lognormal(),
                         data = data_train,
                         newdata = data_test,
                         noncentred = T,
                         burnin = 5000,
                         samples = 5000,
                         adapt_delta = 0.9,
                         thin = 9)

pairs(rainfall_lognormal)
mcmc_plot(rainfall_lognormal, type = "trace", variable = c("rainfall", "ar1[1]", "sigma[1]"))
plot(rainfall_lognormal, type = "forecast")
plot(rainfall_lognormal, type = 'residuals')
pp_check(rainfall_lognormal, type = 'dens_overlay')
pp_check(rainfall_lognormal, type = 'pit_ecdf')
summary(rainfall_lognormal)
mcmc_plot(rainfall_lognormal, variable = c('rainfall'), regex = TRUE, type = 'areas')

# lognormal with a smooth term and season
mixed_lognormal = mvgam(flow ~ s(rainfall, bs = "cc") + s(season, bs = "cc",
                        k = 5),
                           trend_model = "AR1",
                           family = lognormal(),
                           data = data_train,
                           newdata = data_test,
                           noncentred = T,
                           burnin = 5000,
                           samples = 5000,
                           adapt_delta = 0.9,
                           thin = 9)

pairs(mixed_lognormal)
mcmc_plot(mixed_lognormal, type = "trace", variable = c("rainfall", "ar1[1]", "sigma[1]"))
plot(mixed_lognormal, type = "forecast")
plot(mixed_lognormal, type = 'residuals')
pp_check(mixed_lognormal, type = 'dens_overlay')
pp_check(mixed_lognormal, type = 'pit_ecdf')
summary(mixed_lognormal)
mcmc_plot(mixed_lognormal, variable = c('rainfall'), regex = TRUE, type = 'areas')



##################Analysis session 2####################
            


###### lognormal dynamic model (i.e. time-varying predictors, i.e. a state space model) ##### 2

ldyn_model = mvgam(flow ~ dynamic(rainfall, k=40),
                        trend_model = "AR1",
                        family = lognormal(),
                        data = data_train,
                        newdata = data_test,
                        noncentred = T,
                        burnin = 1000,
                        samples = 1000,
                        adapt_delta = 0.9,
                        thin = 2)

mcmc_plot(ldyn_model, type = "trace", variable = c("rainfall", "ar1[1]", "sigma[1]"))
plot(ldyn_model, type = "forecast")
plot(ldyn_model, type = 'residuals')
pp_check(ldyn_model, type = 'dens_overlay')
pp_check(ldyn_model, type = 'pit_ecdf')
summary(ldyn_model)
mcmc_plot(ldyn_model, variable = c('rainfall'), regex = TRUE, type = 'areas')

# lognormal dynamic model (i.e. time-varying predictors, i.e. a state space model) + season

ldyn_model_season = mvgam(flow ~ dynamic(rainfall, k=40) + s(season, bs ="cc"),
                   trend_model = "AR1",
                   family = lognormal(),
                   data = data_train,
                   newdata = data_test,
                   noncentred = T,
                   burnin = 1000,
                   samples = 1000,
                   adapt_delta = 0.9,
                   thin = 2)

mcmc_plot(ldyn_model_season, type = "trace", variable = c("rainfall", "ar1[1]", "sigma[1]"))
plot(ldyn_model_season, type = "forecast")
plot(ldyn_model_season, type = 'residuals')
pp_check(ldyn_model_season, type = 'dens_overlay')
pp_check(ldyn_model_season, type = 'pit_ecdf')
summary(ldyn_model_season)
mcmc_plot(ldyn_model_season, variable = c('rainfall'), regex = TRUE, type = 'areas')

###### lognormal dynamic model with cumulative rainfall over the previous 5 months (i.e. time-varying predictors, i.e. a state space model) ###### 3

ldyn.2_model = mvgam(flow ~ dynamic(cum_rain5, k=40),
                   trend_model = "AR1",
                   family = lognormal(),
                   data = data_train, #once you are comfortable add a                                            larger time period
                   newdata = data_test,
                   noncentred = T,
                   burnin = 1000,
                   samples = 1000,
                   adapt_delta = 0.9,#look up what this is
                   thin = 2)

mcmc_plot(ldyn_model, type = "trace", variable = c("cum_rain5", "ar1[1]", "sigma[1]"))
plot(ldyn_model, type = "forecast")
plot(ldyn_model, type = 'residuals')
pp_check(ldyn_model, type = 'dens_overlay')
pp_check(ldyn_model, type = 'pit_ecdf')
summary(ldyn_model)
mcmc_plot(ldyn_model, variable = c('cum_rain5'), regex = TRUE, type = 'areas')

#season
ldyn_model_season.1 = mvgam(flow ~ dynamic(cum_rain5, k=40) +s(season),
                   trend_model = "AR1",
                   family = lognormal(),
                   data = data_train,
                   newdata = data_test,
                   noncentred = T,
                   burnin = 1000,
                   samples = 1000,
                   adapt_delta = 0.9,
                   thin = 2)

mcmc_plot(ldyn_model_season.1, type = "trace", variable = c("cum_rain5", "ar1[1]", "sigma[1]"))
plot(ldyn_model_season.1, type = "forecast")
plot(ldyn_model_season.1, type = 'residuals')
pp_check(ldyn_model_season.1, type = 'dens_overlay')
pp_check(ldyn_model_season.1, type = 'pit_ecdf')
summary(ldyn_model_season.1)
mcmc_plot(ldyn_model_season.1, variable = c('cum_rain5'), regex = TRUE, type = 'areas')

##### lognormal dynamic model with cumulative rainfall over the previous 5 months (i.e. time-varying predictors, i.e. a state space model) ###### 4
#   - All data
ldyn.3_model = mvgam(flow ~ dynamic(cum_rain5, k=40),
                   trend_model = "AR1",
                   family = lognormal(),
                   data = dat,
                   noncentred = T,
                   burnin = 1000,
                   samples = 1000,
                   adapt_delta = 0.9,
                   thin = 2)

mcmc_plot(ldyn_model, type = "trace", variable = c("cum_rain5", "ar1[1]", "sigma[1]"))
plot(ldyn_model, type = "forecast")
plot(ldyn_model, type = 'residuals')
pp_check(ldyn_model, type = 'dens_overlay')
pp_check(ldyn_model, type = 'pit_ecdf')
summary(ldyn_model)
mcmc_plot(ldyn_model, variable = c('cum_rain5'), regex = TRUE, type = 'areas')

#season 

ldyn.3_model_season = mvgam(flow ~ dynamic(cum_rain5, k=40),
                     trend_model = "AR1",
                     family = lognormal(),
                     data = dat,
                     noncentred = T,
                     burnin = 1000,
                     samples = 1000,
                     adapt_delta = 0.9,
                     thin = 2)

mcmc_plot(ldyn_model, type = "trace", variable = c("cum_rain5", "ar1[1]", "sigma[1]"))
plot(ldyn_model, type = "forecast")
plot(ldyn_model, type = 'residuals')
pp_check(ldyn_model, type = 'dens_overlay')
pp_check(ldyn_model, type = 'pit_ecdf')
summary(ldyn_model)
mcmc_plot(ldyn_model, variable = c('cum_rain5'), regex = TRUE, type = 'areas')