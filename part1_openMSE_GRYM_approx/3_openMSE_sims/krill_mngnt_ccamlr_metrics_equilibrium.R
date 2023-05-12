
# ------------------------------------------------------------------------------
# Performance metrics for krill based on current CCAMLR management (i.e.
# the two  decision rules)

## ------------------------- ##
##   Depletion Probability   ##
## ------------------------- ##

# *Depletion probability*: Probability of lowest spawning biomass over a 20-year
# harvesting period dropping below 20% of its pre-exploitation level

# Following krill's Grym Workshop - the probability that the
# minimum value of SSB over the projection period is less than 20% of SSB0,
# i.e. p[min(SSB)/SSB0 < 0.2]

PD <- function (MSEobj = NULL, Ref = 0.2, Yrs = 20){
  Yrs <- ChkYrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "Depletion: Lowest Spawning Biomass relative to SSB0"
  if (Ref != 1) {
    PMobj@Caption <- paste0("Prob. min(SSB) < ", Ref, " SSB0 (Years ", 
                            Yrs[1], " - ", Yrs[2], ")")
  }
  else {
    PMobj@Caption <- paste0("Prob. min(SSB) < SSB0 (Years ", Yrs[1], 
                            " - ", Yrs[2], ")")
  }
  
  PMobj@Ref <- Ref
  
  # get lowest SSB over the selected window of the projection period, for each
  # simulation and MP
  if (MSEobj@nMPs > 1) mar <- 2 else mar <- 1
  mar <- 1:mar
  SSB_lowest <- apply(MSEobj@SSB[ , ,  Yrs[1]:Yrs[2]], mar, min, na.rm = TRUE)
  
  # step required to conform with PMobj@Stat requirements
  if(MSEobj@nMPs == 1){
    SSB_lowest <- array(SSB_lowest, dim = c(MSEobj@nsim, 1))
  }
  
  PMobj@Stat <- SSB_lowest/MSEobj@OM$SSB0
  
  PMobj@Prob <- PMobj@Stat < PMobj@Ref
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSEobj@MPs
  PMobj
  
}

class(PD) <- "PM"

# library(DLMtool)
# MSE <- runMSE(MPs = "AvC")
# MSE <- runMSE()
# PD(MSE)





## --------------------- ##
##   Escapement in SSB   ##
## --------------------- ##

# Escapement: median SSB at end of a 20-year harvesting period relative to median SSB0,
# i.e. median(SSBF)/median(SSB0)
# Escapement Rule: median escapement at the end of a 20 year period is 75% of
# the median pre-exploitation level

ESC <- function (MSEobj = NULL, Ref = 1, Yrs = 20){
  Yrs <- ChkYrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "Escapement: Final Spawning Biomass relative to SSB0"
  PMobj@Caption <- paste0("median(SSB at Year ", Yrs[2], ")/median(SSB0)")
  
  PMobj@Ref <- Ref
  
  #browser()
  # derive median SSB0 over all simulations
  SSB0_med <- median(MSEobj@OM$SSB0)
  
  # Select SSB in final year of the selected window
  PMobj@Stat <- MSEobj@SSB[, , Yrs[2], drop = FALSE]
  # since size of 3rd dimension is 1 (last year only), calProb merely simplifies array
  PMobj@Prob <- calcProb(PMobj@Stat, MSEobj)
  PMobj@Mean <- apply(PMobj@Stat, 2, median)/SSB0_med
  PMobj@MPs <- MSEobj@MPs
  PMobj
}

class(ESC) <- "PM"