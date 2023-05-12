# ------------------------------------------------------------------------------
# Custom-made MP for krill based current CCAMLR management: constant TAC as a
# proportion (gamma, i.e. the exploitation rate) of an estimate of
# pre-exploitation biomass (B0)
#
# Note: This is only an approximation to current krill management methods.
# Literature and Grym code point to the use of an estimate of mean B0 at a given
# "monitoring period" - the time of the year in which surveys are typically done
# to estimate B0. Population dynamics in DLMtool follow yearly time-steps, so
# there is no obvious way to derive the B0 for a given time period within the
# year

# core function
#' @param gamma double, the harvest rate that, combined with B0, sets the TAC
#' @param B0err numeric vector of size `nsim`, random draws of errors in B0
#'   estimates
gammaB0_ <- function(x, Data, reps = 1, gamma, B0err){

  # create a 'Rec' object
  Rec <- new("Rec")
  
  # get the pre-exploitation biomass for the sought simulation index
  B0 <- Data@OM$B0[x]
  
  # Replicating Grym's approach for setting the catch limit, based on a survey
  # estimate of B0. So, first generate an estimate of B0, accounting for
  # observational error
  B0_hat <- B0 * B0err[x]
  
  # Then compute the catch limit recommendation, and assign it to the TAC slot of
  # 'Rec'
  Rec@TAC <- gamma * B0_hat

  # return the 'Rec' object
  Rec
}



#' Function to build multiple gamma_B0-based MPs for a choice of gammas. Also
#' passing the number of years in historical simulation and the SD of B0 survey
#' estimates.
#' In DLMtool, each MP must be defined as a unique function, and must be of class "MP"
#' @inheritParams gammaB0_
build_gammaB0 <- function(gammas, B0err){
  
  purrr::map_chr(gammas, function(x){
    
    # name of the function
    fn_name <- paste0("gammaB0_", x)
    
    # function re-definition for the gamma value
    fn_def <- purrr::partial(gammaB0_, gamma = !!x, B0err = !!B0err)
    
    # assign function to name
    assign(fn_name, fn_def, envir = .GlobalEnv)
    
    # assign class MP to function
    eval(
      rlang::call2("<-", 
                   rlang::expr(class(!!rlang::sym(fn_name))), "MP"), 
      envir = .GlobalEnv
    )
    
    # return function name for use in MSE run
    fn_name
  })
}

# library(DLMtool)
#gammaB0_MPs <- build_gammaB0(gammas = seq(0, 0.05, by = 0.01))
#MSE <- runMSE(MPs = gammaB0_MPs)
# summary(MSE)





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

#MSE <- runMSE()
#MSE <- runMSE(MPs = "AvC")
#ESC(MSE)
# summary(MSE, "ESC")

