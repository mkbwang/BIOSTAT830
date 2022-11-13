rm(list=ls())

a_t <- exp(5-0.5*sin(seq(1, 100)/(6*pi)))
theta_t <- rep(1, 101)

mu <- 1
lambda <- 1
rho <- 0.6

set.seed(2022)
Bt <- rbeta(n=101, shape1=rho*lambda, shape2=(1-rho)*lambda)
epsilon_t <- rgamma(n=101, shape=(1-rho)*lambda, scale=1/lambda)

for (j in 2:101){
  theta_t[j] <- Bt[j] * theta_t[j-1] + epsilon_t[j]
}

Y_t <- rep(1, 100)

for (j in 1:100){
  Y_t[j] <- rpois(n=1, a_t[j] * theta_t[j+1])
}


result_df <- data.frame(Theta = theta_t[2:101],
                        A_t = a_t,
                        Y = Y_t[1:100],
                    Time = seq(1, 100))



library(ggplot2)

theta_plot <- ggplot(result_df, aes(x=Time, y=Theta)) + geom_point() +
  geom_line() + xlab("Time") + ylab("Theta") +theme_bw()

A_plot <- ggplot(result_df, aes(x=Time, y=A_t)) + geom_point()+
  geom_line() + xlab("Time") + ylab("A_t") + theme_bw()

Y_plot <- ggplot(result_df, aes(x=Time, y=Y)) + geom_point()+
  geom_line() + xlab("Time") + ylab("Y") + theme_bw()

library(cowplot)

combined_plot <- plot_grid(theta_plot, A_plot, Y_plot,
                           nrow=3)


write.csv(result_df, "HW3/P2simulation.csv", row.names=FALSE)

