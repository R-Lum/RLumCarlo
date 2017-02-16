rm(list = ls(all=T))
library(RLumCarlo)

## Fig 1 ----

times <- seq(0, 500) # time = temperature
## Run MC simulation

run_MC_ISO(A = 0.20,
           rho = 0.007,
           clusters = 10,
           times = times) %>%
  calc_RLumCarlo() %>%
    plot_RLumCarlo(norm = T, legend = T)
grid()

## Fig 2 ----


times <- seq(0, 1000) # time = temperature

## Run MC simulation

run_MC_CW_IRSL(A = 0.12, rho = 0.003, times = times) %>%
calc_RLumCarlo() %>% plot_RLumCarlo(norm = T, legend = T)

run_MC_CW_IRSL(A = 0.21, rho = 0.003, times = times) %>%
calc_RLumCarlo() %>% plot_RLumCarlo(norm = T, add = T)
grid()

## Fig 3-----

s <- 3.5e12
rho <- 0.015
E <- 1.45

delta.t <- 1
times <- seq(100, 450, delta.t) # time = temperature

rc0 <- run_MC_TL(s=s, E = E, rho = rho, r_c = 0, times = times)
rc07 <- run_MC_TL(s=s, E = E, rho = rho, r_c = 0.7, times = times)
rc077 <- run_MC_TL(s=s, E = E, rho = rho, r_c = 0.77, times = times)
rc086 <- run_MC_TL(s=s, E = E, rho = rho, r_c = 0.86, times = times)



## Plot average signal

results_rc0 <- calc_RLumCarlo(results = rc0)
results_rc07 <- calc_RLumCarlo(results = rc07)
results_rc077 <- calc_RLumCarlo(results = rc077)
results_rc086 <- calc_RLumCarlo(results = rc086)


plot(
  x = times,
  y = results_rc0$avg/max(results_rc0$avg),
  type = "b",
  ylab = "normalized TL signal",
  xlab = "Temperature [°C]"
)
lines(
  x = times,
  y = results_rc07$avg/max(results_rc0$avg),
  type = "p",
  pch = 6,
  col = "green"
)
lines(
  x = times,
  y = results_rc077$avg/max(results_rc0$avg),
  type = "p",
  pch = 4,
  col = "blue"
)
lines(
  x = times,
  y = results_rc086$avg/max(results_rc0$avg),
  type = "b",
  pch = 5,
  col = "red"
)

legend("topright",
       legend = c("0", "5", "10", "50"),
       pch = c(1,6,5,4),
       col = c("black", "green", "blue", "red"))

grid()

## Fig 4 ----------

s <- 3.5e12
rho <- 0.015
E <- 1.45

delta.t <- 1
times <- seq(200, 500, delta.t) # time = temperature

run_MC_TL(s=s, E = E, rho = rho, r_c = 0.85, times = times) %>%
  calc_RLumCarlo() %>% plot_RLumCarlo(legend = T)

run_MC_TL(s=s, E = E, rho = rho, r_c = 1.13, times = times) %>%
  calc_RLumCarlo() %>% plot_RLumCarlo(add = T)

run_MC_TL(s=s, E = E, rho = rho, r_c = 1.3, times = times) %>%
  calc_RLumCarlo() %>% plot_RLumCarlo(add = T)

grid()
