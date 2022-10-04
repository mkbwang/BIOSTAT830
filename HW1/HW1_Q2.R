library(deSolve)
library(dplyr)

params1 <- c(beta=5/14, delta=1/3, gamma=1/14)
params2 <- c(beta=5/7, delta=1/3, gamma=1/14)
params3 <- c(beta=15/14, delta=1/3, gamma=1/14)

state <- c(S=1e6, E=0, I=30, R=0)

times <- seq(0, 360, by=0.1)

SEIR_model <- function(t, state, parameters){
  with(as.list(c(state, parameters)),{
      dS <- -beta * S * I / (S+E+I+R)
      dE <- beta * S * I / (S+E+I+R) - delta * E
      dI <- delta * E - gamma * I
      dR <- gamma * I
      list(c(dS, dE, dI, dR))
  })
}

out_1 <- ode(y=state, times=times, func=SEIR_model, parms = params1)
out_2 <- ode(y=state, times=times, func=SEIR_model, parms = params2)
out_3 <- ode(y=state, times=times, func=SEIR_model, parms = params3)

out_1_thinning <- out_1[seq(1, 3601, 10), ]
out_2_thinning <- out_2[seq(1, 3601, 10), ]
out_3_thinning <- out_3[seq(1, 3601, 10), ]

combined_output_halfyear <- rbind(out_1_thinning[1:180, ], out_2_thinning[1:180, ],
                                  out_3_thinning[1:180, ]) %>%
  as.data.frame()
combined_output_halfyear$R0 <- rep(c(5, 10, 15), each=180)
combined_output_halfyear$R0 <- as.factor(combined_output_halfyear$R0)

combined_output_fullyear <- rbind(out_1_thinning, out_2_thinning,
                                  out_3_thinning) %>%
  as.data.frame()
combined_output_fullyear$R0 <- rep(c(5, 10, 15), each=361)
combined_output_fullyear$Re <- combined_output_fullyear$R0 * combined_output_fullyear$S / 1e6
combined_output_fullyear$R0 <- as.factor(combined_output_fullyear$R0)

library(ggplot2)

S_traj <- ggplot(combined_output_halfyear, aes(x=time, y=S, linetype=R0)) +
  geom_path() + xlab("Day") + ylab("S") +
  theme_bw() + theme(text = element_text(size = 12))

E_traj <- ggplot(combined_output_halfyear, aes(x=time, y=E, linetype=R0)) +
  geom_path() + xlab("Day") + ylab("E") +
  theme_bw() + theme(text = element_text(size = 12))

I_traj <- ggplot(combined_output_halfyear, aes(x=time, y=I, linetype=R0)) +
  geom_path() + xlab("Day") + ylab("I") +
  theme_bw() + theme(text = element_text(size = 12))

R_traj <- ggplot(combined_output_halfyear, aes(x=time, y=R, linetype=R0)) +
  geom_path() + xlab("Day") + ylab("R") +
  theme_bw() + theme(text = element_text(size = 12))

Re_traj <- ggplot(combined_output_fullyear, aes(x=time, y=Re, linetype=R0))+
  geom_path() + xlab("Day") + ylab("Effective Reproduction Number") +
  theme_bw() + theme(text = element_text(size = 12))

library(cowplot)

plot_grid(S_traj, E_traj, I_traj, R_traj, Re_traj, ncol=2)
