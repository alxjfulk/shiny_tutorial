library(deSolve)
sir <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I
    dI <- beta * S * I - gamma * I
    dR <- gamma * I
    
    return(list(c(dS, dI, dR)))
  })
}
init <- c(S = 1-1e-6, I = 1e-6, 0.0)
parameters <- c(beta = 1.4247, gamma = 0.14286)
times <- seq(0, 70, by = 1)
out <- as.data.frame(ode(y = init, times = times, func = sir, parms = parameters))
out$time <- NULL
  
matplot(times, out, type = "l", xlab = "Time", ylab = "S, I, R Proportions", main = "SIR Model", lwd = 1, lty = 1, bty = "l", col = 2:4)

