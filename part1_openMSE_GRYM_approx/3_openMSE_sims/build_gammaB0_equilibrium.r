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
