xt <- df_inf_sens$time

plot(dgamma(xt, shape=5.807, scale=0.948))
lines(dweibull(xt, shape=1.58, scale=7.11))
lines(dweibull(xt, shape=1.6, scale=6.84))
lines(df_inf_sens$infectiousness)
lines(df_inf_sens$sensitivity)
df_inf_sens

# PCR Test sensitivity for time since infection
test_sens <- c(0.333333333, 0.454545455, 0.875, 
               0.96, 0.96, 0.92, 0.76, 0.64, 0.56, 0.25, 0.181818182, 0.045454545, 0,0)
test_sens_fun <- smooth.spline(seq(0,length(test_sens)-1,by=1), test_sens, spar=0.3)

pcr_test_sens <- c(0.666666667, 0.727272727, 1, 1, 1, 1, 1, 0.96, 0.88, 
                   0.791666667, 0.909090909, 0.727272727, 0.9, 0.8, 0.526315789, 
                   0.384615385, 0,0)
pcr_test_sens_fun <- smooth.spline(seq(0,length(pcr_test_sens)-1,by=1), pcr_test_sens, spar=0.3)

path <- "/Users/tm-pham/surfdrive/PHD/Utrecht/JuliusCenter/COVID-19_school_transmission/literature/infectiousness/hellewell_bmcMed_2021/"
test_sens_tab <- read.csv(paste0(path, "LFT_curve_summary.csv"), sep=",")
test_sens_fun_hl <- smooth.spline(test_sens_tab$days_since_infection, test_sens_tab$upper_95, spar=0.3)

pcr_sens_tab <- read.csv(paste0(path, "PCR_curve_summary.csv"), sep=",")
pcr_sens_fun_hl <- smooth.spline(pcr_sens_tab$days_since_infection, pcr_sens_tab$upper_95, spar=0.3)

# Data frame with infectiousness and test sensitivity for every hour
time_vec_hour <- seq(0,15, by=1/24)
df_inf_sens = as.data.frame(cbind(time=time_vec_hour, 
                                  infectiousness=predict(infectivity_fun, time_vec_hour)$y,
                                  lft_sens_hl = predict(test_sens_fun_hl, time_vec_hour)$y,
                                  pcr_sens_hl = predict(pcr_sens_fun_hl, time_vec_hour)$y,
                                  antigen_sens=predict(test_sens_fun, time_vec_hour)$y, 
                                  pcr_sens=predict(pcr_test_sens_fun, time_vec_hour)$y))
df_inf_sens[,-1] <- apply(df_inf_sens[,-1], 2, function(x) ifelse(x<0, 0, x)) # Force negative values to be zero
df_inf_sens[,-1] <- apply(df_inf_sens[,-1], 2, function(x) ifelse(x>1, 1, x))

library(tidyverse)
df_plot <- as.data.frame(cbind(time = df_inf_sens$time, 
                               lft_sens_hl = df_inf_sens$lft_sens_hl,
                               pcr_sens_hl = df_inf_sens$pcr_sens_hl,
                               antigen_sens = df_inf_sens$antigen_sens, 
                               pcr_sens = df_inf_sens$pcr_sens, 
                               inc_gamma = dgamma(time_vec_hour, shape=5.807, scale=0.948),
                               inc_weibull = dweibull(time_vec_hour, shape=1.58, scale=7.11),
                               gen = dweibull(time_vec_hour, shape=1.6, scale=6.84),
                               inf_He = df_inf_sens$infectiousness))

plot_data <- reshape2::melt(df_plot, id.vars=c("time"))
colnames(plot_data) <- c("time", "var", "value")
plot_data$time <- as.numeric(plot_data$time)
plot_data$value <- as.numeric(plot_data$value)

ggplot(plot_data, aes(x=time, y=value, colour=var)) + 
  geom_line()


df_plot <- as.data.frame(cbind(df_inf_sens, 
                   inc_gamma = dgamma(time_vec_hour, shape=5.807, scale=0.948),
                   inc_weibull = dweibull(time_vec_hour, shape=1.58, scale=7.11)))
plot_data <- reshape2::melt(df_plot, id.vars=c("time"))
colnames(plot_data) <- c("time", "var", "value")
plot_data$time <- as.numeric(plot_data$time)
plot_data$value <- as.numeric(plot_data$value)

ggplot(plot_data, aes(x=time, y=value, colour=var)) + 
  geom_line()
