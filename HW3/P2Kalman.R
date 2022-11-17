rm(list=ls())


a_t <- exp(5-0.5*sin(seq(1, 100)/(6*pi)))
lambda <- 1
rho <- 0.6
theta_0 <- 1
m_0 <- theta_0
C_0 <- 0

data <- read.csv('HW3/P2simulation.csv')
Y <- data$Y

f <- Y
C <- rep(0, 100)
# DmBeta <- matrix(0, N,  k)
# S <- matrix(0,  k,  k )

Q <- c(rep(0,100))
m <- rep(1,100)
###########################################################
#########kalman filter: forward calculations###############
###########################################################

############### initializations ##########################

f[1] <- rho * a_t[1] * m_0 + (1 - rho) * a_t[1]
u <- rho^2 * C_0 + (1 - rho^2)/lambda  ##u is u_0
Q[1] <- a_t[1] * a_t[1] * u + a_t[1]
C[1] <- u/(1 + a_t[1] * u)
m[1] <- rho*m_0 + 1 - rho + C[1]*(Y[1] - f[1])

#DmBeta[1,] <- - a[1] * C[1] * X[1,]

################## recursion ##############################

for(n in 2:100){
  f[n] <-  rho * a_t[n] * m[n-1] + (1 - rho) * a_t[n]
  u <- rho^2 * C[n - 1] + (1 - rho^2)/lambda
  ### u is u[n-1]
  Q[n] <- a_t[n] * a_t[n] * u  +  a_t[n]
  C[n] <- u / (1 + a_t[n] * u)
  m[n] <- rho * m[n-1] + 1 - rho + C[n] * (Y[n] - f[n])
}


m_forward <- m
##############################################################
#############kalman smoother: backward calculations###########
##############################################################

############## initializations ###############################

ms0 <- m_0 #note that C0 = 0 from smoother formula of ms
ms <- m
Cs <- C
# DmsBeta <- DmBeta

############### recursion ###################################

for(n in (100 - 1):1){
  u <-  C[n] * rho^2 + (1 - rho^2)/lambda
  ms[n] <- m[n] + rho * C[n] * (ms[n + 1] - rho * m[n] - 1 + rho)/u
  Cs[n] <- (1 - rho^2) * C[n] / (u*lambda) + (rho^2)*C[n]^2 * Cs[n + 1]/u^2
}


data$Mf <- m_forward
data$Ms <- ms

values <- c(data$Theta, data$Mf, data$Ms)
time <- rep(seq(1, 100), 3)
Type <- rep(c("True", "Kalman Filter", "Kalman Smoother"), each=100)
result <- data.frame(time, values, Type)

ggplot(result, aes(x=time, y=values)) + geom_point(alpha=0.5) +
  geom_line(alpha=0.5) +xlab("Time") + ylab("Theta") +
  facet_wrap(~Type, ncol=1) + theme_bw()

#cat("solving S ... ")

################################################################
###################Newton Scoring algorithm#####################
################################################################

#####Using Kalman smoother to approximate the latent process.###
# psi <- as.vector(t(X) %*% as.matrix(Y - a * ms))
#
# beta <- beta - solve(S, psi)
#
# a <- as.vector(exp( X %*% as.matrix(beta) ))
#
# cat("max(abs(psi)) =", max(abs(psi)), "\n")
#
# dif <- beta - bet
# cat("max(abs(dif)) =", max(abs(dif)), "\n")
#
# epsilon <- max(abs(dif))  #compatible with the second criterion.
# bet <- beta

##############################################################
#######################Estimate sigmasq########################
###############################################################
#########sigmasq is estimated based on momont##################.

# sigmasq <- sum((Y - a)^2 - a)/sum(a^2)
#
# cat("sigmasq =", sigmasq, "\n")

###############################################################
#######################Estimate alpha##########################
###############################################################
###the first version for alpha estimation#####don't work#######
#o1 <- (sigmasq * a[1:(N-1)])^{-1} + 1
#o2 <- (sigmasq * a[2:N])^{-1}+ 1
#alpha <- rhoy * mean(sqrt(o1 * o2))

######the second version of alpha estimation###################
#adjust <-
#      alpha * C[1:(N-1)]*Cs[2:N]/(C[1:(N-1)] * alpha^2 - (1 - alpha^2)*sigmasq)
#alpha <- (mean((ms[1:(N-1)]-1)*(ms[2:N]-1)) + mean(adjust))/sigmasq

######the third version for alpha estimation##################
#u <- alpha * C + (1 - alpha^2)*sigmasq
#alpha <- mean((ms[1:(N-1)]-1)*(ms[2:N]-1)/(sigmasq - C[1:(N-1)]*Cs[2:N]/u[1:(N-1)]))
# alpha <-acf(m,1,"correlation",plot=F)$acf[2, ,1]
# cat("alpha =", alpha, "\n")

