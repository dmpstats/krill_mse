
library(Grym)
library(DLMtool)
library(ggplot2)


source("krill_recruitment/prfit.R")
rec_pars <- readRDS("krill_recruitment/Rvar_and_M_draws_Prop_Recruit.rds")


nsim <- 5000
grym_idx <- 3

# ------------------------------------------------------------------------------------ #
#        Checking coherence in recruitment simulations between MSETool and GRYM        #
# ------------------------------------------------------------------------------------ #

# GRYM-based recruitment simulation for one draw of recruitment variability and
# M (derived from the proportional recruitment approach)
#
# Assuming lognormal distributed recruits
#
# random no. of recruits, with mean 1 and the correct coefficient of variation 
R_grym <- recLogNormal(n = nsim*3, mn = rec_pars$pars$mnQ[grym_idx], vr = rec_pars$pars$vrQ[grym_idx])

mean(R_grym)
hist(R_grym)



# MSEtool-based recruitment simulation for one draw of recruitment variability and
# M (derived from the proportional recruitment approach)

# Import the Operating Model from excel workbook
OM_krill_grym <- XL2OM("OM_krill_grym_approx.xlsx")

# This is required to switch off age-plus group
OM_krill_grym@cpars$plusgroup <- 0

# R0 seems to set mean recruitment when steepness is 1
OM_krill_grym@R0
  
OM_krill_grym@nsim <- nsim

# set custom parameters for M
OM_krill_grym@cpars$M <- rep(rec_pars$pars$M[grym_idx], OM_krill_grym@nsim)

# converting CV to SD of a lognormal distribution
OM_krill_grym@cpars$Perr <- rep(sqrt(log(rec_pars$pars$CV[grym_idx]^2 + 1)), OM_krill_grym@nsim) 


# Simulate fishery history
OM_krill_grym_hist <- Simulate(OM_krill_grym)

# Historic annual recruitment, by simulation (as used by markdown documentation)
R_mse <- apply(OM_krill_grym_hist@AtAge$Number[, 1, , ], 1:2, sum)

mean(R_mse)
hist(R_mse)




# Graphical comparison

R_mse <- data.frame(
  source = "MSEtool",
  R = as.vector(R_mse)
)

R_grym <- data.frame(
  source = "Grym",
  R = R_grym
) 


ggplot() +
  geom_histogram(data = R_mse, aes(R), col = "red", fill = "red", alpha = 0.2) +
  geom_histogram(data = R_grym, aes(R), col = "blue", fill = "blue", alpha = 0.2) +
  theme_bw()


