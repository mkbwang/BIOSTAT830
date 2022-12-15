

# serial interval probability mass
lnmean <- 4.7 # Nishiura etal result
lnsd <- 2.9


## derive mu and sigma of log normal distribution
sigma_sq <- log(1+lnsd^2/lnmean^2)
sigma <- sqrt(sigma_sq)
mu <- log(4.7) - sigma_sq/2

## calculate discrete pmf for serial interval 1-14
w_s <- rep(0, 14)
w_s[1] <- plnorm(1.5, meanlog=mu, sdlog=sigma)
for (j in 2:14){
  w_s[j] <- plnorm(j+0.5, meanlog=mu, sdlog=sigma) - plnorm(j-0.5, meanlog=mu, sdlog=sigma)
}
w_s <- w_s * 1/sum(w_s)

w_s_df <- data.frame(SI=seq(1, 14), weights=w_s)
# w_s <- readRDS('Project/processed_data/w_s_prob.rds')
w_s_df$percentage <- sprintf('%.2f%%', w_s*100)
weight_plot <- ggplot(w_s_df, aes(x=SI, y=weights)) +
  geom_bar(stat="identity", color="black", fill="white", width = 0.5, position = position_dodge(width=0.3))+
  geom_text(aes(label=percentage), vjust=-0.3, size=3, position = position_dodge(width=0.3))+
  ylim(0, 0.22)+ scale_x_continuous(breaks=seq(1, 14))+
  xlab("Serial Interval (Days)") + ylab("Probability")

saveRDS(w_s, file.path('Project', 'processed_data', 'w_s_prob.rds'))

