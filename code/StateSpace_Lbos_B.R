##############################################################################
### Building a state-space model to explore streamflow at Jonkershoek
### For data see: Slingsby, Jasper A., Abri Buys, Adrian D. A. Simmers, Eric Prinsloo, Greg G. Forsyth, Julia Glenday, and Nicky Allsopp. 2021. “Jonkershoek: Africa’s Oldest Catchment Experiment ‐ 80 Years and Counting.” Hydrological Processes, February. https://doi.org/10.1002/hyp.14101.
### Based on this tutorial: https://course.naturecast.org/lessons/r-state-space-models-1/r_tutorial_mvgam/
##############################################################################

library(mvgam)
library(tidyverse)
library(slider)
library(ggpubr)

# Get data
flow <- read_csv("data/Lambrechtbos_B_Belfort_stream_flow_volume_March_1961_October_2008.csv")
#flow2 <- read_csv("data/Langrivier_Belfort_stream_flow_volume_March_1961_October_2008.csv")
rain <- read_csv("data/10B_manual_monthly_rainfall_Apr1944_Mar1991.csv")

#flow <- bind_rows(list(tibble(flow,Site = "LbosB"), tibble(flow2,Site = "Langrivier")))

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

# Some exploratory data analysis
plot_mvgam_series(data = dat, y = 'flow')

dat %>% ggplot(aes(y = flow, x = rainfall)) +
  geom_point() +
  facet_wrap(.~season)

dat %>% pivot_longer(cols = c("flow", "rainfall")) %>%
  ggplot(aes(y = value, x = time)) +
  geom_line() +
  facet_wrap(.~name, nrow = 2, scales = "free")

# Explore lagged or cumulative effects of rainfall (also see RcppRoll::roll_mean or roll_sum that allow weighted cumulative windows)
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

# gaussian
baseline_model = mvgam(flow ~ rainfall,
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

# lognormal
lognormal_model = mvgam(flow ~ rainfall,
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

# lognormal dynamic model (i.e. time-varying predictors, i.e. a state space model)

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


# lognormal dynamic model with cumulative rainfall over the previous 5 months (i.e. time-varying predictors, i.e. a state space model)

ldyn_model = mvgam(flow ~ dynamic(cum_rain5, k=40),
                   trend_model = "AR1",
                   family = lognormal(),
                   data = data_train,
                   newdata = data_test,
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


# lognormal dynamic model with cumulative rainfall over the previous 5 months (i.e. time-varying predictors, i.e. a state space model)
#   - All data
ldyn_model = mvgam(flow ~ dynamic(cum_rain5, k=40),
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

# lognormal dynamic model with seasonality using cumulative rainfall over the previous 5 months (i.e. time-varying predictors, i.e. a state space model)
#   - All data
ldyn_model = mvgam(flow ~ dynamic(cum_rain5, k=40) +
                     s(season),
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
