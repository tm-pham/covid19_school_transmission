df <- cbind(incubation_period=dweibull(df_inf_sens$time, shape=1.58, scale=7.11), df_inf_sens)

# Plot infectiousness and test sensitivity
df_wide <- reshape2::melt(df[df$time<=16,], id="time")
head(df_wide)
plot <- ggplot(df_wide, aes(x=time, y=value, colour=variable)) + 
  geom_line(size=1.1) + 
  labs(x="Time since infection", y="Probability") + 
  # scale_x_continuous(breaks=time_steps) + 
  theme_publication() + 
  theme(legend.title = element_blank(), 
        legend.text = element_text(size=16))
plot
ggsave(plot, file="../results/figures/inf_sens_plot.pdf", width=16, height=9)
