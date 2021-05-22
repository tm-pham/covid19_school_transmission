population_size <- 17280397

# Incidence
infectious_prev <- read.csv("../data/model_input/COVID-19_prevalentie.csv")
infectious_prev$Date <- as.Date(infectious_prev$Date, format="%Y-%m-%d")
infectious_prev$prev_avg <- as.numeric(infectious_prev$prev_avg)
infectious_prev <- infectious_prev[,c("Date", "prev_low", "prev_avg", "prev_up")]

0.07*summary(infectious_prev$prev_avg)/population_size
0.07*max(infectious_prev$prev_avg, na.rm = T)/population_size


df_infectious_prev <- melt(infectious_prev, id=c("Date"))
head(df_infectious_prev)

d1 <- as.Date("2021-03-29", format="%Y-%m-%d")
d2 <- as.Date("2021-04-01", format="%Y-%m-%d")
prop_pos_tested_child <- 0.14
prop_pos_tested_child*max(df_infectious_prev[df_infectious_prev$Date>=d1 & df_infectious_prev$Date<=d2,"value"]/population_size)

ggplot(df_infectious_prev, aes(x=Date, y=value, color=variable)) + 
  geom_line()


incidence <- sapply(2:length(infectious_prev$prev_avg), function(i) infectious_prev$prev_avg[i]-infectious_prev$prev_avg[i-1])
infectious_inc <- as.data.frame(cbind(Date=as.Date(infectious_prev[2:nrow(infectious_prev),"Date"], format="%Y-%m-%d"), inc=as.numeric(incidence)))
summary(incidence/population_size)
max(incidence/population_size, na.rm = T)
