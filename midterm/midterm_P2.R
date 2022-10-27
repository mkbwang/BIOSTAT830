
library(deSolve)

rm(list=ls())
initial_state <- c(S=1e6-50, E=0, I=50, R=0)

real_params <- c(beta=5/7, delta=1/3, gamma=1/14, N=1e6)

times <- seq(0, 90, by=1)


SEIR_model <- function(t, state, parameters){
  with(as.list(c(state, parameters)),{
    dS <- -beta * S * I / N
    dE <- beta * S * I / N - delta * E
    dI <- delta * E - gamma * I
    dR <- gamma * I
    list(c(dS, dE, dI, dR))
  })
}

simulated_data <- rk4(y=initial_state,
                      times=times,
                      func = SEIR_model,
                      parms=real_params)

simulated_data <- round(simulated_data, digits = 0)


## search parameter space

beta_space <- seq(0.01, 1, 0.01)

loss.sse <- function(beta){
  params <- c(beta=beta, delta=1/3, gamma=1/14, N=1e6)
  out <- rk4(y=initial_state,
      times=times,
      func = SEIR_model,
      parms=params)
  out <- round(out, digits=0)
  residual <- out[, 'I'] - simulated_data[, 'I']
  sum(residual^2)
}

SSE <- sapply(beta_space, loss.sse)
minSSE <- min(SSE)
SSE_df <- data.frame(beta_hat = seq(0.01, 1, 0.01),
                     loss = SSE)

SSE_df$Type <- "No Underreporting"

library(ggplot2)
SSE_trajectory <- ggplot(SSE_df, aes(x=beta_hat, y=loss))+ geom_line() + geom_point(size=1.2)+
  xlab("Beta") + ylab("SSE of Infectious Compartment Trajectory") + ylim(0, 5.6e12)+
  geom_hline(yintercept=minSSE, linetype="dashed",
             color = "red", size=1.2)+
  theme_bw()


## SEIR with underreporting


initial_state_all <- c(S=1e6-50, E=0, I=50, OI=0,  R=0)
initial_state_observed <- c(S=1e6-50, E=0, I=50,  R=0)

uSEIR_generation_model <- function(t, state, parameters){
  with(as.list(c(state, parameters)),{
    dS <- -beta * S * I / N
    dE <- beta * S * I / N - delta * E
    dI <- delta * E - gamma * I
    dOI <- 0.9 * (delta * E - gamma * I)
    dR <- gamma * I
    list(c(dS, dE, dI, dOI, dR))
  })
}

uSEIR_simulated_data <- rk4(y=initial_state_all,
                      times=times,
                      func = uSEIR_generation_model,
                      parms=real_params)
uSEIR_simulated_data <- round(uSEIR_simulated_data, digits=0)

uSEIR_estimation_model <- function(t, state, parameters){
  with(as.list(c(state, parameters)),{
    dS <- - 10 / 9 *beta * S * I / N
    dE <- 10 / 9 * beta * S * I / N - delta * E
    dI <- 9 / 10 * delta * E - gamma * I
    dR <- 1 / 10 * delta * E + gamma * I
    list(c(dS, dE, dI, dR))
  })
}

uSEIR.loss.sse <- function(beta){
  params <- c(beta=beta, delta=1/3, gamma=1/14, N=1e6)
  out <- rk4(y=initial_state_observed,
             times=times,
             func = uSEIR_estimation_model,
             parms=params)
  out <- round(out, digits=0)
  residual <- out[, 'I'] - uSEIR_simulated_data[, 'OI']
  sum(residual^2)
}

uSEIR_SSE <- sapply(beta_space, uSEIR.loss.sse)
uSEIR_minSSE <- min(uSEIR_SSE)
uSEIR_SSE_df <- data.frame(beta_hat = seq(0.01, 1, 0.01),
                     loss = uSEIR_SSE)
uSEIR_SSE_df$Type <- "Underreporting"

combined_performance <- rbind(SSE_df, uSEIR_SSE_df)

uSEIR_SSE_trajectory <- ggplot(uSEIR_SSE_df, aes(x=beta_hat, y=loss))+ geom_line() + geom_point(size=1.2)+
  xlab("Beta") + ylab("SSE of Infectious Compartment Trajectory") + ylim(0, 5.6e12)+
  geom_hline(yintercept=uSEIR_minSSE, linetype="dashed",
             color = "red", size=1.2)+
  theme_bw()


library(cowplot)

plot_grid(SSE_trajectory, uSEIR_SSE_trajectory, labels="AUTO",
          ncol=2)
