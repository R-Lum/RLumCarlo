library(RLumCarlo)
library(FME)

data_exp <- run_MC_CW_IRSL(A = 0.12, rho = 0.003, times = seq(0,1000,1)) %>% calc_RLumCarlo()
# data_exp <- data_exp[,c(4,1,2,3)]
data_exp[,2] <- data_exp[,2]/max(data_exp[,2])
data_exp[,3] <- data_exp[,3]/max(data_exp[,3])
data_exp[,1] <- data_exp[,1]/max(data_exp[,1])
# names(data_exp) <- c("time", "avg", "y_min", "y_max")


run_model <- function(parms){

  A <- unname(parms["A"])
  rho <- 0.003#unname(parms["rho"])

  model <- run_MC_CW_IRSL(A = A, rho = rho, times = seq(0,1000,1)) %>% calc_RLumCarlo()
  # model <- model[,c(4,1,2,3)]
  model[,2] <- model[,2]/max(model[,2])
  model[,3] <- model[,3]/max(model[,3])
  model[,4] <- model[,1]/max(model[,1])
  # names(model) <- c("time", "avg", "y_min", "y_max")

  return(model)

}

## sens range ----
# parRanges <- data.frame(min = c(0.01, 0.001), max = c(0.1, 0.01))
# rownames(parRanges)<- c("A", "rho")
#
# global_Sens <- sensRange(func = run_model,
#                    parms = c(A = 0.20, rho = 0.007),
#                    parRange = parRanges,
#                    num = 50,
#                    sensvar = "avg")
#
# plot(summary(global_Sens))
#
# local_sens <- sensFun(func = run_model, parms = c(A = 0.20, rho = 0.007), varscale = 1, senspar = "A")
# plot(local_sens)

### FIT ----

# initial guess
parms <- c(A = 0.1)

## function calculates differences between data_exp and model output
Objective <- function(x, parset = names(x)) {
  parms[parset] <- x
  print(parms)

  model <- run_model(parms)

  return(modCost(obs = data_exp, model = model))
}

## Fitting function
Fit <- modFit(p = parms, f = Objective, method = "Port", lower = c(0.0001))
summary(Fit)

## plot results
plot(
  data_exp[,c(4,1)], type = "l")

A = coef(Fit)["A"]
rho = 0.003#coef(Fit)["rho"]

lines(x = data_exp$time,
      y = run_model(parms = c(A, rho))$avg,
      col = "green")
legend("topright", legend = c("exp", "fit"), lwd = 1, col = c("black", "green"))

cat("expected A:", 0.12, " fitted A:", A)

